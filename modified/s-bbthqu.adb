------------------------------------------------------------------------------
--                                                                          --
--                  GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                --
--                                                                          --
--               S Y S T E M . B B . T H R E A D S . Q U E U E S            --
--                                                                          --
--                                  B o d y                                 --
--                                                                          --
--        Copyright (C) 1999-2002 Universidad Politecnica de Madrid         --
--             Copyright (C) 2003-2005 The European Space Agency            --
--                     Copyright (C) 2003-2018, AdaCore                     --
--                                                                          --
-- GNARL is free software; you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion. GNARL is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.                                     --
--                                                                          --
--                                                                          --
--                                                                          --
--                                                                          --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
-- GNARL was developed by the GNARL team at Florida State University.       --
-- Extensive contributions were provided by Ada Core Technologies, Inc.     --
--                                                                          --
-- The port of GNARL to bare board targets was initially developed by the   --
-- Real-Time Systems Group at the Technical University of Madrid.           --
--                                                                          --
------------------------------------------------------------------------------

pragma Restrictions (No_Elaboration_Code);
with System.IO;
with System.BB.Time; use System.BB.Time;
with CPU_Budget_Monitor;
with Mixed_Criticality_System;

pragma Warnings (Off);
with Ada.Text_IO;
pragma Warnings (On);

package body System.BB.Threads.Queues is

   use System.Multiprocessors;
   use System.BB.Board_Support.Multiprocessors;

   ----------------
   -- Local data --
   ----------------

   Alarms_Table : array (CPU) of Thread_Id := (others => Null_Thread_Id);
   pragma Volatile_Components (Alarms_Table);
   --  Identifier of the thread that is in the first place of the alarm queue

   type Log_Record is
      record
         ID : Integer;
         DM : Integer;
         Runs : Integer;
         Preemption : Integer;
         Min_Response_Jitter :  System.BB.Time.Time_Span;
         Max_Response_Jitter :  System.BB.Time.Time_Span;
         Min_Release_Jitter :  System.BB.Time.Time_Span;
         Max_Release_Jitter :  System.BB.Time.Time_Span;
         Average_Response_Jitter : System.BB.Time.Time_Span;
      end record;

   type Array_Log_Record is array (1 .. 90) of Log_Record;

   Log_Table : Array_Log_Record;
   Max_ID_Table : Integer := 0;

   procedure Initialize_Log_Table (ID : Integer) is
   begin
      if ID /= 0 then
         Log_Table (ID) := (ID, 0, -1, 0,
                             System.BB.Time.Time_Span_Last,
                             System.BB.Time.Time_Span_First,
                             System.BB.Time.Time_Span_Last,
                             System.BB.Time.Time_Span_First,
                             System.BB.Time.Time_Span_Zero);
         if Max_ID_Table < ID then
            Max_ID_Table := ID;
         end if;
      end if;
   end Initialize_Log_Table;

   procedure Add_DM (ID : Integer) is
   begin
      if ID /= 0 then
         Log_Table (ID).DM := Log_Table (ID).DM + 1;
      end if;
   end Add_DM;

   procedure Add_Runs (ID : Integer) is
   begin
      if ID /= 0 then
         Log_Table (ID).Runs :=
           Log_Table (ID).Runs + 1;
      end if;
   end Add_Runs;

   procedure Add_Preemption (ID : Integer) is
   begin
      if ID /= 0 then
         Log_Table (ID).Preemption := Log_Table (ID).Preemption + 1;
      end if;
   end Add_Preemption;

   procedure Print_Log (First_Index : Integer) is
      i : Integer := First_Index;
   begin
      while i <= Max_ID_Table loop
         System.IO.Put ("Tab;");
         System.IO.Put (Integer'Image (i));
         System.IO.Put (Integer'Image (Log_Table (i).DM));
         System.IO.Put (Integer'Image (Log_Table (i).Runs));
         System.IO.Put (Integer'Image (Log_Table (i).Preemption));
         System.IO.Put_Line ("");
         i := i + 1;
      end loop;

   end Print_Log;

   ---------------------
   -- Change_Priority --
   ---------------------

   procedure Change_Priority (Thread : Thread_Id; Priority : Integer)
   is
      CPU_Id       : constant CPU := BOSUMU.Current_CPU;
      Head         : Thread_Id;
      Prev_Pointer : Thread_Id;

   begin
      --  A CPU can only change the priority of its own tasks

      pragma Assert (CPU_Id = Get_CPU (Thread));

      --  Return now if there is no change. This is a rather common case, as
      --  it happens if user is not using priorities, or if the priority of
      --  an interrupt handler is the same as the priority of the interrupt.
      --  In any case, the check is quick enough.

      if Thread.Active_Priority = Priority then
         return;
      end if;

      --  Change the active priority. The base priority does not change

      Thread.Active_Priority := Priority;

      --  Outside of the executive kernel, the running thread is also the first
      --  thread in the First_Thread_Table list. This is also true in general
      --  within the kernel, except during transcient period when a task is
      --  extracted from the list (blocked by a delay until or on an entry),
      --  when a task is inserted (after a wakeup), after a yield or after
      --  this procedure. But then a context_switch put things in order.

      --  However, on ARM Cortex-M, context switches can be delayed by
      --  interrupts. They are performed via a special interrupt (Pend_SV),
      --  which is at the lowest priority. This has three consequences:
      --   A) it is not possible to have tasks in the Interrupt_Priority range
      --   B) the head of First_Thread_Table list may be different from the
      --      running thread within user interrupt handler
      --   C) the running thread may not be in the First_Thread_Table list.
      --  The following scenario shows case B: while a thread is running, an
      --  interrupt awakes a task at a higher priority; it is put in front of
      --  the First_Thread_Table queue, and a context switch is requested. But
      --  before the end of the interrupt, another interrupt triggers. It
      --  increases the priority of  the current thread, which is not the
      --  first in queue.
      --  The following scenario shows case C: a task is executing a delay
      --  until and therefore it is removed from the First_Thread_Table. But
      --  before the context switch, an interrupt triggers and change the
      --  priority of the running thread.

      --  First, find THREAD in the queue and remove it temporarly.

      Head := First_Thread_Table (CPU_Id);

      if Head = Thread then

         --  This is the very common case: THREAD is the first in the queue

         if Thread.Next = Null_Thread_Id
           or else Priority >= Thread.Next.Active_Priority
         then
            --  Already at the right place.
            return;
         end if;

         --  Remove THREAD from the queue

         Head := Thread.Next;
      else

         --  Uncommon case: less than 0.1% on a Cortex-M test.

         --  Search the thread before THREAD.

         Prev_Pointer := Head;
         loop
            if Prev_Pointer = null then
               --  THREAD is not in the queue. This corresponds to case B.
               return;
            end if;

            exit when Prev_Pointer.Next = Thread;

            Prev_Pointer := Prev_Pointer.Next;
         end loop;

         --  Remove THREAD from the queue.

         Prev_Pointer.Next := Thread.Next;
      end if;

      --  Now insert THREAD.

      --  FIFO_Within_Priorities dispatching policy. In ALRM D.2.2 it is
      --  said that when the active priority is lowered due to the loss of
      --  inherited priority (the only possible case within the Ravenscar
      --  profile) the task is added at the head of the ready queue for
      --  its new active priority.

      if Priority >= Head.Active_Priority then

         --  THREAD is the highest priority thread, so put it in the front of
         --  the queue.

         Thread.Next := Head;
         Head := Thread;
      else

         --  Search the right place in the queue.

         Prev_Pointer := Head;
         while Prev_Pointer.Next /= Null_Thread_Id
           and then Priority < Prev_Pointer.Next.Active_Priority
         loop
            Prev_Pointer := Prev_Pointer.Next;
         end loop;

         Thread.Next := Prev_Pointer.Next;
         Prev_Pointer.Next := Thread;
      end if;

      First_Thread_Table (CPU_Id) := Head;
   end Change_Priority;

   ---------------------------
   -- Change_Fake_Number_ID --
   ---------------------------

   procedure Change_Fake_Number_ID
     (Thread       : Thread_Id;
      Fake_Number_ID : Integer)
   is
   begin
      Thread.Fake_Number_ID := Fake_Number_ID;
   end Change_Fake_Number_ID;

   ------------------------
   -- Change_Is_Sporadic --
   ------------------------

   procedure Change_Is_Sporadic
     (Thread       : Thread_Id;
      Bool : Boolean)
   is
   begin
      Thread.Is_Sporadic := Bool;
   end Change_Is_Sporadic;

   ------------------------------
   -- Change_Relative_Deadline --
   ------------------------------

   procedure Change_Relative_Deadline
     (Thread       : Thread_Id;
      Rel_Deadline : System.BB.Deadlines.Relative_Deadline;
      Is_Floor     : Boolean)  --  useless for FPS, set what you want
   is
      pragma Unreferenced (Is_Floor);
      CPU_Id      : constant CPU := Get_CPU (Thread);
   begin
      --  A CPU can only change the relative deadline of its own tasks

      pragma Assert (CPU_Id = Current_CPU);

      --  We can only change the priority of the thread that is
      --  currently executing.

      pragma Assert (Thread = Running_Thread_Table (CPU_Id));

      --  Change the active relative deadline. The base relative deadline does
      --  not change
      Thread.Active_Relative_Deadline := Rel_Deadline;

      if Thread.Active_Relative_Deadline <= Thread.Active_Period then
         Change_Absolute_Deadline (Thread, System.BB.Time.Time_First +
                                   Thread.Active_Starting_Time -
                     (Thread.Active_Period - Thread.Active_Relative_Deadline)
                                  + Global_Interrupt_Delay);
      else
         Change_Absolute_Deadline (Thread, System.BB.Time.Time_First +
                                   Thread.Active_Starting_Time +
                     (Thread.Active_Relative_Deadline - Thread.Active_Period)
                                    + Global_Interrupt_Delay);

      end if;

   end Change_Relative_Deadline;

   -------------------
   -- Change_Period --
   -------------------

   procedure Change_Period
     (Thread       : Thread_Id;
      Period       : System.BB.Time.Time_Span)
   is
      CPU_Id      : constant CPU := Get_CPU (Thread);
   begin
      pragma Assert (CPU_Id = Current_CPU);
      pragma Assert (Thread = Running_Thread_Table (CPU_Id));
      Thread.Active_Period := Period;
   end Change_Period;

   --------------------------
   -- Change_Starting_Time --
   --------------------------

   procedure Change_Starting_Time
     (Thread        : Thread_Id;
      Starting_Time : System.BB.Time.Time_Span)
   is
      CPU_Id      : constant CPU := Get_CPU (Thread);
   begin
      pragma Assert (CPU_Id = Current_CPU);
      pragma Assert (Thread = Running_Thread_Table (CPU_Id));
      Thread.Active_Starting_Time := Starting_Time;
      Thread.Active_Next_Period := System.BB.Time.Time_First +
          (Starting_Time - Thread.Active_Period);
   end Change_Starting_Time;

   ---------------------------
   -- Change_Release_Jitter --
   ---------------------------

   procedure Change_Release_Jitter
     (Thread        : Thread_Id)
   is
      CPU_Id      : constant CPU := Get_CPU (Thread);
      Temp : System.BB.Time.Time_Span;
   begin
      pragma Assert (CPU_Id = Current_CPU);
      pragma Assert (Thread = Running_Thread_Table (CPU_Id));

      if Thread.Just_Wakeup = True then
         Temp := System.BB.Time.Clock - Thread.Active_Next_Period;
         Thread.Active_Release_Jitter := System.BB.Time.Time_First + (Temp);
         Thread.Just_Wakeup := False;
      end if;
   end Change_Release_Jitter;

   -----------------
   -- Set_Jitters --
   -----------------

   procedure Update_Jitters
     (Thread      : Thread_Id;
      Response_Jitter : System.BB.Time.Time_Span;
      Release_Jitter : System.BB.Time.Time_Span)
   is
      CPU_Id      : constant CPU := Get_CPU (Thread);
   begin
      pragma Assert (CPU_Id = Current_CPU);
      pragma Assert (Thread = Running_Thread_Table (CPU_Id));

      if Log_Table (Thread.Fake_Number_ID).Average_Response_Jitter
        = System.BB.Time.Time_Span_Zero
      then
         Log_Table (Thread.Fake_Number_ID).Average_Response_Jitter :=
           Response_Jitter;
      else
         Log_Table (Thread.Fake_Number_ID).Average_Response_Jitter :=
           ((Log_Table (Thread.Fake_Number_ID).Average_Response_Jitter *
              Log_Table (Thread.Fake_Number_ID).Runs) +
              Response_Jitter)
           / (Log_Table (Thread.Fake_Number_ID).Runs + 1);
      end if;

      if Response_Jitter <
        Log_Table (Thread.Fake_Number_ID).Min_Response_Jitter
      then
         Log_Table (Thread.Fake_Number_ID).Min_Response_Jitter :=
           Response_Jitter;
      end if;

      if Response_Jitter >
        Log_Table (Thread.Fake_Number_ID).Max_Response_Jitter
      then
         Log_Table (Thread.Fake_Number_ID).Max_Response_Jitter :=
           Response_Jitter;
      end if;

      if Release_Jitter <
        Log_Table (Thread.Fake_Number_ID).Min_Release_Jitter
      then
         Log_Table (Thread.Fake_Number_ID).Min_Release_Jitter :=
           Release_Jitter;
      end if;

      if Release_Jitter >
        Log_Table (Thread.Fake_Number_ID).Max_Release_Jitter
      then
         Log_Table (Thread.Fake_Number_ID).Max_Release_Jitter :=
           Release_Jitter;
      end if;

   end Update_Jitters;

   ------------------------------
   -- Change_Absolute_Deadline --
   ------------------------------

   procedure Change_Absolute_Deadline
     (Thread       : Thread_Id;
      Abs_Deadline : System.BB.Deadlines.Absolute_Deadline)
   is
      --  Previous_Thread, Next_Thread : Thread_Id;
      CPU_Id      : constant CPU := Get_CPU (Thread);

   begin
      --  A CPU can only change the absolute deadline of its own tasks
      pragma Assert (CPU_Id = Current_CPU);

      pragma Assert (Thread = Running_Thread_Table (CPU_Id));

      Thread.Active_Absolute_Deadline := Abs_Deadline;

   end Change_Absolute_Deadline;

   ---------------------------
   -- Context_Switch_Needed --
   ---------------------------

   function Context_Switch_Needed return Boolean is
      --   Now : System.BB.Time.Time;
   begin
      --  A context switch is needed when there is a higher priority task ready
      --  to execute. It means that First_Thread is not null and it is not
      --  equal to the task currently executing (Running_Thread).

      if First_Thread /= Running_Thread and Running_Thread.Preemption_Needed
      then
         Add_Preemption (Running_Thread.Fake_Number_ID);
      end if;

      return First_Thread /= Running_Thread;
   end Context_Switch_Needed;

   ----------------------
   -- Current_Priority --
   ----------------------

   function Current_Priority
     (CPU_Id : System.Multiprocessors.CPU) return Integer
   is
      Thread : constant Thread_Id := Running_Thread_Table (CPU_Id);
   begin
      if Thread = null or else Thread.State /= Threads.Runnable then
         return System.Any_Priority'First;
      else
         return Thread.Active_Priority;
      end if;
   end Current_Priority;

   -------------
   -- Extract --
   -------------

   procedure Extract (Thread : Thread_Id) is
      CPU_Id : constant CPU := Get_CPU (Thread);
   begin
      --  A CPU can only modify its own tasks queues

      pragma Assert (CPU_Id = Current_CPU);

      First_Thread_Table (CPU_Id) := Thread.Next;
      Thread.Next := Null_Thread_Id;
   end Extract;

   -------------------------
   -- Extract_First_Alarm --
   -------------------------

   function Extract_First_Alarm return Thread_Id is
      CPU_Id : constant CPU       := Current_CPU;
      Result : constant Thread_Id := Alarms_Table (CPU_Id);

   begin
      --  A CPU can only modify its own tasks queues

      pragma Assert (CPU_Id = Current_CPU);

      Alarms_Table (CPU_Id) := Result.Next_Alarm;
      Result.Alarm_Time := System.BB.Time.Time'Last;
      Result.Next_Alarm := Null_Thread_Id;
      return Result;
   end Extract_First_Alarm;

   ------------------
   -- First_Thread --
   ------------------

   function First_Thread return Thread_Id is
   begin
      return First_Thread_Table (Current_CPU);
   end First_Thread;

   -------------------------
   -- Get_Next_Alarm_Time --
   -------------------------

   function Get_Next_Alarm_Time (CPU_Id : CPU) return System.BB.Time.Time is
      Thread : Thread_Id;

   begin
      Thread := Alarms_Table (CPU_Id);

      if Thread = Null_Thread_Id then

         --  If alarm queue is empty then next alarm to raise will be Time'Last

         return System.BB.Time.Time'Last;

      else
         return Thread.Alarm_Time;
      end if;
   end Get_Next_Alarm_Time;

   ------------
   -- Insert --
   ------------

   procedure Insert (Thread : Thread_Id) is
      Aux_Pointer : Thread_Id;
      CPU_Id      : constant CPU := Get_CPU (Thread);

   begin
      --  ??? This pragma is disabled because the Tasks_Activated only
      --  represents the end of activation for one package not all the
      --  packages. We have to find a better milestone for the end of
      --  tasks activation.

      --  --  A CPU can only insert alarm in its own queue, except during
      --  --  initialization.

      --  pragma Assert (CPU_Id = Current_CPU or else not Tasks_Activated);

      --  It may be the case that we try to insert a task that is already in
      --  the queue. This can only happen if the task was not runnable and its
      --  context was being used for handling an interrupt. Hence, if the task
      --  is already in the queue and we try to insert it, we need to check
      --  whether it is in the correct place.

      --  No insertion if the task is already at the head of the queue

      if First_Thread_Table (CPU_Id) = Thread then
         null;

      --  Insert at the head of queue if there is no other thread with a higher
      --  priority.

      elsif First_Thread_Table (CPU_Id) = Null_Thread_Id
        or else
          Thread.Active_Priority > First_Thread_Table (CPU_Id).Active_Priority
      then
         Thread.Next := First_Thread_Table (CPU_Id);
         First_Thread_Table (CPU_Id) := Thread;

      --  Middle or tail insertion

      else
         --  Look for the Aux_Pointer to insert the thread just after it

         Aux_Pointer := First_Thread_Table (CPU_Id);
         while Aux_Pointer.Next /= Null_Thread_Id
           and then Aux_Pointer.Next /= Thread
           and then Aux_Pointer.Next.Active_Priority >= Thread.Active_Priority
         loop
            Aux_Pointer := Aux_Pointer.Next;
         end loop;

         --  If we found the thread already in the queue, then we need to move
         --  it to its right place.

         if Aux_Pointer.Next = Thread then

            --  Extract it from its current location

            Aux_Pointer.Next := Thread.Next;

            --  Look for the Aux_Pointer to insert the thread just after it

            while Aux_Pointer.Next /= Null_Thread_Id
              and then
                Aux_Pointer.Next.Active_Priority >= Thread.Active_Priority
            loop
               Aux_Pointer := Aux_Pointer.Next;
            end loop;
         end if;

         --  Insert the thread after the Aux_Pointer

         Thread.Next := Aux_Pointer.Next;
         Aux_Pointer.Next := Thread;
      end if;
   end Insert;

   ------------------
   -- Insert_Alarm --
   ------------------

   procedure Insert_Alarm
     (T        : System.BB.Time.Time;
      Thread   : Thread_Id;
      Is_First : out Boolean)
   is
      CPU_Id       : constant CPU := Get_CPU (Thread);
      Alarm_Id_Aux : Thread_Id;

   begin
      --  A CPU can only insert alarm in its own queue

      pragma Assert (CPU_Id = Current_CPU);

      --  Set the Alarm_Time within the thread descriptor

      Thread.Alarm_Time := T;

      --  Case of empty queue, or new alarm expires earlier, insert the thread
      --  as the first thread.

      if Alarms_Table (CPU_Id) = Null_Thread_Id
        or else T < Alarms_Table (CPU_Id).Alarm_Time
      then
         Thread.Next_Alarm := Alarms_Table (CPU_Id);
         Alarms_Table (CPU_Id) := Thread;
         Is_First := True;

      --  Otherwise, place in the middle

      else
         --  Find the minimum greater than T alarm within the alarm queue

         Alarm_Id_Aux := Alarms_Table (CPU_Id);
         while Alarm_Id_Aux.Next_Alarm /= Null_Thread_Id and then
           Alarm_Id_Aux.Next_Alarm.Alarm_Time < T
         loop
            Alarm_Id_Aux := Alarm_Id_Aux.Next_Alarm;
         end loop;

         Thread.Next_Alarm := Alarm_Id_Aux.Next_Alarm;
         Alarm_Id_Aux.Next_Alarm := Thread;

         Is_First := False;
      end if;
   end Insert_Alarm;

   --------------------
   -- Running_Thread --
   --------------------

   function Running_Thread return Thread_Id is
   begin
      return Running_Thread_Table (Current_CPU);
   end Running_Thread;

   ---------------------------
   -- Wakeup_Expired_Alarms --
   ---------------------------

   procedure Wakeup_Expired_Alarms (Now : Time.Time) is

      CPU_Id        : constant CPU := Current_CPU;
      Wakeup_Thread : Thread_Id;

   begin
      --  Extract all the threads whose delay has expired

      while Get_Next_Alarm_Time (CPU_Id) <= Now loop

         --  Extract the task(s) that was waiting in the alarm queue and insert
         --  it in the ready queue.

         Wakeup_Thread := Extract_First_Alarm;

         --  We can only awake tasks that are delay statement

         pragma Assert (Wakeup_Thread.State = Delayed);

         Wakeup_Thread.State := Runnable;

         Wakeup_Thread.Preemption_Needed := True;

         Change_Absolute_Deadline (Wakeup_Thread,
                                   (Wakeup_Thread.Active_Period +
                                      Wakeup_Thread.Active_Absolute_Deadline));

         Wakeup_Thread.Just_Wakeup := True;
         Wakeup_Thread.Active_Next_Period := Wakeup_Thread.Active_Next_Period
           + Wakeup_Thread.Active_Period;

         Insert (Wakeup_Thread);

      end loop;

      --  Note: the caller (BB.Time.Alarm_Handler) must set the next alarm
   end Wakeup_Expired_Alarms;

   -----------
   -- Yield --
   -----------

   procedure Yield (Thread : Thread_Id) is
      CPU_Id      : constant CPU     := Get_CPU (Thread);
      Prio        : constant Integer := Thread.Active_Priority;
      Aux_Pointer : Thread_Id;
      Cancelled   : Boolean;
      pragma Unreferenced (Cancelled);
   --   Now         : System.BB.Time.Time;
   begin
      --  A CPU can only modify its own tasks queues

      pragma Assert (CPU_Id = Current_CPU);

      Thread.Just_Wakeup := True;
      Thread.Active_Next_Period := Thread.Active_Next_Period +
        Thread.Active_Period;

      if Thread.Next /= Null_Thread_Id
        and then Thread.Next.Active_Priority = Prio
      then
         --  Stop budget monitoring.
         CPU_Budget_Monitor.Clear_Monitor (Cancelled);

         First_Thread_Table (CPU_Id) := Thread.Next;

         --  Look for the Aux_Pointer to insert the thread just after it

         Aux_Pointer  := First_Thread_Table (CPU_Id);
         while Aux_Pointer.Next /= Null_Thread_Id
           and then Prio = Aux_Pointer.Next.Active_Priority
         loop
            Aux_Pointer := Aux_Pointer.Next;
         end loop;

         --  Insert the thread after the Aux_Pointer

         Thread.Next := Aux_Pointer.Next;
         Aux_Pointer.Next := Thread;
      end if;

   end Yield;

   ------------------
   -- Queue_Length --
   ------------------

   function Queue_Length return Natural is
      Res : Natural   := 0;
      T   : Thread_Id := First_Thread_Table (Current_CPU);

   begin
      while T /= null loop
         Res := Res + 1;
         T := T.Next;
      end loop;

      return Res;
   end Queue_Length;

   -------------------
   -- Queue_Ordered --
   -------------------

   function Queue_Ordered return Boolean is
      T : Thread_Id := First_Thread_Table (Current_CPU);
      N : Thread_Id;

   begin
      if T = Null_Thread_Id then
         --  True if the queue is empty
         return True;
      end if;

      loop
         N := T.Next;
         if N = Null_Thread_Id then
            --  True if at end of the queue
            return True;
         end if;

         if T.Active_Priority < N.Active_Priority then
            return False;
         end if;

         T := N;
      end loop;
   end Queue_Ordered;

   ------------------
   --  Set_Budget  --
   ------------------

   procedure Set_Budget
     (Thread : Thread_Id;
      Budget : System.BB.Time.Time_Span) is
   begin
      Thread.Budget := Budget;
      Thread.Is_Monitored := True;
   end Set_Budget;

   procedure Insert_Discarded (Thread : Thread_Id) is
   begin
      Thread.Next := Discarded_Thread_Table;
      Discarded_Thread_Table := Thread;
   end Insert_Discarded;

   --------------------
   --  Print_Queues  --
   --------------------

   procedure Print_Queues is
      Aux_Pointer : Thread_Id := Discarded_Thread_Table;
      T2 : Integer := -100;
   begin

      while Aux_Pointer /= Null_Thread_Id
      loop
         T2 := Aux_Pointer.Base_Priority;
         Ada.Text_IO.Put_Line (Integer'Image (T2) & " is DISCARDED");
         Aux_Pointer := Aux_Pointer.Next;
      end loop;
   end Print_Queues;

   -------------------------------
   --  Initialize_LO_Crit_Task  --
   -------------------------------

   procedure Initialize_LO_Crit_Task
      (Thread : Thread_Id;
      Is_Migrable : Boolean) is
      use Mixed_Criticality_System;
   begin
      Thread.Criticality_Level := LOW;
      Thread.Is_Migrable := Is_Migrable;
   end Initialize_LO_Crit_Task;

   -------------------------------
   --  Initialize_HI_Crit_Task  --
   -------------------------------

   procedure Initialize_HI_Crit_Task
      (Thread : Thread_Id) is
      use Mixed_Criticality_System;
   begin
      Thread.Criticality_Level := HIGH;
   end Initialize_HI_Crit_Task;

   ---------------------
   --  Discard_Tasks  --
   ---------------------

   procedure Discard_Tasks is
      CPU_Id       : constant CPU := Current_CPU;
      Aux_Pointer  : Thread_Id    := First_Thread_Table (CPU_Id);
      Curr_Pointer : Thread_Id    := First_Thread_Table (CPU_Id);
      Prev_Pointer : Thread_Id    := Null_Thread_Id;
   begin

      --  First extract from READY queue
      while Curr_Pointer /= Null_Thread_Id
      loop
            null;
            if Curr_Pointer.Is_Migrable then

               if Curr_Pointer = First_Thread_Table (CPU_Id) then
                  --  The first thread is migrable, so it must be removed.
                  --  This means that the second thread in the queue,
                  --  i.e. Curr_Pointer.Next, must be set
                  --  as the first thread in the queue.
                  First_Thread_Table (CPU_Id) := Curr_Pointer.Next;
               else
                  --  We have to remove a thread between two others
                  --  (the last one could be the Null thread).
                  --  This means that the previous thread in the queue
                  --  must be linked to the last one.
                  Prev_Pointer.Next := Curr_Pointer.Next;
               end if;

               --  Go ahead with the aux pointer.
               Aux_Pointer := Aux_Pointer.Next;

               --  Isolate the current thread.
               Curr_Pointer.Next := Null_Thread_Id;

               --  Insert in the Discarded queue.
               Curr_Pointer.State := Discarded;
               Insert_Discarded (Curr_Pointer);

               Print_Queues;

               --  Go ahead with the current pointer.
               Curr_Pointer := Aux_Pointer;

            else --  Current thread is NOT migrable
               --  then go ahead normally
               Prev_Pointer := Curr_Pointer;
               Aux_Pointer := Aux_Pointer.Next;
               Curr_Pointer := Aux_Pointer;
            end if;
      end loop;

   end Discard_Tasks;

end System.BB.Threads.Queues;
