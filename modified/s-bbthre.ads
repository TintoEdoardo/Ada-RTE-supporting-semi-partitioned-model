------------------------------------------------------------------------------
--                                                                          --
--                  GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                --
--                                                                          --
--                       S Y S T E M . B B . T H R E A D S                  --
--                                                                          --
--                                  S p e c                                 --
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

--  Package that implements basic tasking functionalities

pragma Restrictions (No_Elaboration_Code);

with System;
with System.Storage_Elements;
with System.BB.CPU_Primitives;
with System.BB.Time;
with System.BB.Board_Support;
with System.Multiprocessors;
with System.BB.Deadlines;
with Mixed_Criticality_System;

package System.BB.Threads is
   pragma Preelaborate;

   use type System.Multiprocessors.CPU;
   package MCS renames Mixed_Criticality_System;

   --------------------------
   -- Basic thread support --
   --------------------------

   Initialized : Boolean := False;
   --  Boolean that indicates whether the tasking executive has finished its
   --  initialization.

   type Thread_Descriptor;
   --  This type contains the information about a thread

   type Thread_Id is access all Thread_Descriptor;
   --  Type used as thread identifier

   Null_Thread_Id : constant Thread_Id := null;
   --  Identifier used to define an invalid value for a thread identifier

   type Thread_States is (Runnable, Suspended, Delayed, Discarded);
   --  These are the three possible states for a thread under the Ravenscar
   --  profile restrictions: Runnable (not blocked, and it may also be
   --  executing), Suspended (waiting on an entry call), and Delayed (waiting
   --  on a delay until statement).

   type Array_CPUs is array (System.Multiprocessors.CPU) of Natural;

   --  Addition for MCS by Xu & Burns: a thread is Discarded when is going to
   --  be inserted in the discarded queue (Discarded_Thread_Table).

   type Task_MCS_Data is record
      Id : Natural;

      Hosting_Migrating_Tasks_Priority : Integer;
      On_Target_Core_Priority : Integer;

      Reduced_Period : System.BB.Time.Time_Span;
      Reduced_Absolute_Deadline : System.BB.Deadlines.Absolute_Deadline;
      --  @TODO: they seems not necessary...

      Stored_Absolute_Deadline : System.BB.Deadlines.Absolute_Deadline;
      --  Needed to restore a migrating task's Active_Absoluted_Deadline
      --  once it is restored on its Base_CPU.
   end record;

   type Task_Data_Log is record
      --  BE is Budget_Exceeded
      Times_BE                : Natural           := 0;
      --  Times_BE_On_Target_Core : Natural           := 0;
      Times_Discarded         : Natural           := 0;
      Times_Migrated          : Natural           := 0;
      Times_Restored          : Natural           := 0;
      --  Times_On_CPUs    : Array_CPUs               := (others => 0);
      Locked_Time      : System.BB.Time.Time_Span := 0;
      Last_Time_Locked : System.BB.Time.Time      := 0;
   end record;

   type Thread_Descriptor is record

      Context : aliased System.BB.CPU_Primitives.Context_Buffer;
      --  Location where the hardware registers (stack pointer, program
      --  counter, ...) are stored. This field supports context switches among
      --  threads.

      --  It is important that the Context field is placed at the beginning of
      --  the record, because this assumption is using for implementing context
      --  switching. Take into account the alignment (8 bytes, 64 bits) to
      --  compute the required size.

      ATCB : System.Address;
      --  Address of the Ada Task Control Block corresponding to the Ada task
      --  that executes on this thread.

      Base_CPU : System.Multiprocessors.CPU_Range;
      --  CPU affinity of the thread

      Base_Priority : Integer;
      --  Base priority of the thread. It is used during steady mode.

      --  Hosting_Migrating_Tasks_Priority : Integer;
      --  Priority value used when the current core c_i is hosting the migratin
      --  tasks of the other core c_j, i.e. c_i is in LO-crit mode, while
      --  c_j is in HI-crit one. According to Xu & Burns, this state is
      --  referred as Y(j)_i.

      --  On_Target_Core_Priority : Integer;
      --  Priority valued used when this thread is hosted on the migrating core
      --  , i.e. the one which is not the original one. This happens when
      --  Base_CPU is NOT Active_CPU

      Active_Priority : Integer;
      pragma Volatile (Active_Priority);
      --  Active priority that differs from the base priority due to dynamic
      --  priority changes required by the Ceiling Priority Protocol.
      --  This field is marked as Volatile for a fast implementation
      --  of Get_Priority.

      Top_Of_Stack : System.Address;
      --  Address of the top of the stack that is used by the thread

      Bottom_Of_Stack : System.Address;
      --  Address of the bottom of the stack that is used by the thread

      Next : Thread_Id;
      --  Points to the ready thread that is in the next position for
      --  execution.

      Alarm_Time : System.BB.Time.Time;
      --  Time (absolute) when the alarm for this thread expires

      Next_Alarm : Thread_Id;
      --  Next thread in the alarm queue. The queue is ordered by expiration
      --  times. The first place is occupied by the thread which must be
      --  first awaken.

      State : Thread_States;
      --  Encodes some basic information about the state of a thread

      In_Interrupt : Boolean;
      pragma Volatile (In_Interrupt);
      --  True iff this task has been interrupted, and an interrupt handler
      --  is being executed.

      Wakeup_Signaled : Boolean;
      --  Variable which reflects whether another thread has performed a
      --  Wakeup operation on the thread. It may happen when a task is about
      --  to suspend itself, but it is preempted just before by the task that
      --  is going to awake it.

      Global_List : Thread_Id;
      --  Next thread in the global list. The queue is ordered by creation
      --  time. The first place is occupied by the environment thread, and
      --  it links all threads in the system.

      Execution_Time : System.BB.Time.Composite_Execution_Time;
      --  CPU time spent for this thread

      Active_Relative_Deadline : System.BB.Deadlines.Relative_Deadline;
      pragma Volatile (Active_Relative_Deadline);
      --  Active relative deadline: differs (as priority does) from base
      --  relative deadline due to changes required by the Floor Deadline
      --  Protocol. As done for Priority, this field is marked Volatile for a
      --  fast implementation of Get_Relative_Deadline method.

      Active_Absolute_Deadline : System.BB.Deadlines.Absolute_Deadline;
      pragma Volatile (Active_Absolute_Deadline);
      --  Active absolute deadline differs from active relative deadline
      --  because it rappresents an absolute time value: it is updated every
      --  time a thread is appened in the ready queue because absolute
      --  deadline is the comparison value for correct queue order

      --  Deadlines_Concerning_Migration : Task_Deadlines;

      Preemption_Needed : Boolean := False;
      --  Boolean that indicates whether an urgent task after a wakeup needs to
      --  preempt the running task before its natural suspension.

      Active_Period : System.BB.Time.Time_Span;

      Active_Starting_Time :  System.BB.Time.Time_Span;

      Fake_Number_ID : Integer := -1;

      Is_Sporadic : Boolean := False;

      Just_Wakeup : Boolean := False;

      Active_Next_Period : System.BB.Time.Time :=
        System.BB.Time.Time_First;
      pragma Volatile (Active_Next_Period);

      Active_Release_Jitter : System.BB.Time.Time :=
        System.BB.Time.Time_First;
      pragma Volatile (Active_Release_Jitter);

      First_Execution : Boolean := False;

      --  Additions for MCS by Xu & Burns

      --  CPU runtime (if migrations have been made).
      Active_CPU : System.Multiprocessors.CPU_Range;

      Period : System.BB.Time.Time_Span;

      --  Statically-assigned budget
      Low_Critical_Budget  : System.BB.Time.Time_Span;
      High_Critical_Budget : System.BB.Time.Time_Span;

      --  Run-time budget based on core criticality mode.
      --    1. If criticality mode is LOW, then Budget := Low_Critical_Budget.
      --    2. If criticality mode is HIGH  && Task.Criticality_Level is HIGH,
      --         then Budget := High_Critical_Budget.
      Active_Budget : System.BB.Time.Time_Span;

      Is_Monitored : Boolean := False;

      Criticality_Level : MCS.Criticality;

      --  Next thread in HI-crit queue.
      --  This fields is set iff Thread.Criticality_Level is HIGH.
      Next_HI_Crit : Thread_Id;

      Is_Migrable : Boolean := False;

      --  The following two fields are needed by monitored (Is_Monitored) tasks
      --  Last time that this threads takes the CPU.
      T_Start : System.BB.Time.Time;

      --  Last time that this threads loses/yields the CPU.
      T_Clear : System.BB.Time.Time;

      Log_Table : Task_Data_Log;

      Data_Concerning_Migration : Task_MCS_Data;

      First_Time_On_Delay_Until : Boolean := True;

      --------------------------------
      --  Support for defer freeze  --
      --------------------------------

      Resource_Nesting : Integer := 0;

   end record;

   function Get_Affinity
     (Thread : Thread_Id) return System.Multiprocessors.CPU_Range with
   --  Return CPU affinity of the given thread (maybe Not_A_Specific_CPU)

     Pre => Thread /= Null_Thread_Id,

     Inline => True;

   function Get_CPU
     (Thread : Thread_Id) return System.Multiprocessors.CPU with
   --  Return the CPU in charge of the given thread (always a valid CPU)

     Pre => Thread /= Null_Thread_Id,

     Inline => True;

   procedure Initialize
     (Environment_Thread : Thread_Id;
      Main_Priority      : System.Any_Priority) with
   --  Procedure to initialize the board and the data structures related to the
   --  low level tasking system. This procedure must be called before any other
   --  tasking operation. The operations to perform are:
   --    - Hardware initialization
   --       * Any board-specific initialization
   --       * Interrupts
   --       * Timer
   --    - Initialize stacks for main procedures to be executed on slave CPUs
   --    - Initialize the thread descriptor for the environment task
   --       * Set base CPU for the environment task to the one on which this
   --         initialization code executes
   --       * Set the base and active priority of the environment task
   --       * Store the boundaries of the stack for the environment task
   --       * Initialize the register context
   --    - Initialize the global queues
   --       * Set the environment task as first (and only at this moment) in
   --         the ready queue
   --       * Set the environment task as first (and only at this moment) in
   --         the global list of tasks
   --       * Set the environment task as the currently executing task
   --    - Initialize the floating point unit
   --    - Signal the flag corresponding to the initialization

     Pre =>

       --  This procedure must be called by the master CPU

       Board_Support.Multiprocessors.Current_CPU = Multiprocessors.CPU'First

       --  Initialization can only happen once

       and then not Initialized;

   procedure Initialize_Slave
     (Idle_Thread   : Thread_Id;
      Idle_Priority : Integer;
      Stack_Address : System.Address;
      Stack_Size    : System.Storage_Elements.Storage_Offset) with
   --  Procedure to initialize the idle thread on a slave CPU.
   --  This thread is used to handle interrupt if the CPU doesn't have any
   --  other task. The initialization for the main CPU must have been
   --  performed. The operations to perform are:
   --    - Initialize the thread descriptor
   --       * Set base CPU to the one on which this code executes
   --       * Set the base and active priority
   --       * Store the boundaries of the stack
   --       * Initialize the register context
   --    - Initialize the global queues
   --       * Set the task as the currently executing task in this processor.

     Pre =>

       --  It must happen after the initialization of the master CPU

       Initialized;

   procedure Thread_Create
     (Id            : Thread_Id;
      Code          : System.Address;
      Arg           : System.Address;
      Priority      : Integer;
      Base_CPU      : System.Multiprocessors.CPU_Range;
      Stack_Address : System.Address;
      Stack_Size    : System.Storage_Elements.Storage_Offset) with
   --  Create a new thread
   --
   --  The new thread executes the code at address Code and using Args as
   --  argument. Priority is the base priority of the new thread. The new
   --  thread is provided with a stack of size Stack_Size that has been
   --  preallocated at Stack_Address.
   --
   --  A procedure to destroy threads is not available because that is not
   --  allowed by the Ravenscar profile.

     Pre => Initialized;

   function Thread_Self return Thread_Id with
   --  Return the thread identifier of the calling thread

     Post => Thread_Self'Result /= Null_Thread_Id,

     Inline => True;

   ----------------
   -- Scheduling --
   ----------------

   procedure Set_Priority (Priority : Integer);
   pragma Inline (Set_Priority);
   --  Set the active priority of the executing thread to the given value

   function Get_Priority  (Id : Thread_Id) return Integer with
   --  Get the current active priority of any thread

     Pre => Id /= Null_Thread_Id,

     Inline => True;

   procedure Set_Fake_Number_ID
          (Fake_Number_ID : Integer);
   pragma Inline (Set_Fake_Number_ID);
   --  Set a fake integer number ID

   procedure Set_Is_Sporadic (Bool : Boolean);

   procedure Set_Relative_Deadline
     (Rel_Deadline : System.BB.Deadlines.Relative_Deadline;
      Is_Floor     : Boolean);
   --  pragma Inline (Set_Relative_Deadline);
   --  Set the active relative deadline of the executing thread to the
   --  given value

   procedure Set_Period
     (Period       : System.BB.Time.Time_Span);

   procedure Set_Starting_Time
     (Starting_Time :  System.BB.Time.Time_Span);

   procedure Sleep;
   --  The calling thread is unconditionally suspended. In the case when there
   --  is a request to wakeup the caller just before the state changed to
   --  Suspended then the situation is signaled with the flag Wakeup_Signaled,
   --  and the call to Sleep consumes this token and the state remains
   --  Runnable.

   procedure Wakeup (Id : Thread_Id) with
   --  Thread Id becomes ready (the thread must be previously suspended). In
   --  the case when there is a request to wakeup the caller just before the
   --  state changed to Suspended then the situation is signaled with the
   --  flag Wakeup_Signaled (the state remains unchanged in this case).

     Pre =>
       Id /= Null_Thread_Id

       --  We can only wakeup a task that is already suspended or about to be
       --  suspended (and hence still runnable).

       and then Id.all.State in Suspended | Runnable

       --  Any wakeup previously signaled must have been consumed

       and then not Id.all.Wakeup_Signaled;

   ----------
   -- ATCB --
   ----------

   procedure Set_ATCB (Id : Thread_Id; ATCB : System.Address);
   pragma Inline (Set_ATCB);
   --  This procedure sets the ATCB passed as argument for the thread ID

   function Get_ATCB return System.Address;
   pragma Inline (Get_ATCB);
   --  Returns the ATCB of the currently executing thread

   -----------------------
   -- Additions for MCS --
   -----------------------

   -------------------------------
   --  Initialize_LO_Crit_Task  --
   -------------------------------

   procedure Initialize_LO_Crit_Task
     (Task_Id : Natural;
      LO_Crit_Budget : System.BB.Time.Time_Span;
        Hosting_Migrating_Tasks_Priority : Integer;
        On_Target_Core_Priority : Integer;
      Period : Natural;
      Reduced_Deadline : Natural;
        Is_Migrable : Boolean);

   -------------------------------
   --  Initialize_HI_Crit_Task  --
   -------------------------------

   procedure Initialize_HI_Crit_Task
     (Task_Id : Natural;
      LO_Crit_Budget : System.BB.Time.Time_Span;
       HI_Crit_Budget : System.BB.Time.Time_Span;
       Hosting_Migrating_Tasks_Priority : Integer;
       Period : Natural);

end System.BB.Threads;
