pragma Warnings (Off);
with Ada.Text_IO;
pragma Warnings (On);

with System.BB.Protection;
with System.BB.Board_Support;
with System.BB.Threads.Queues;
with Mixed_Criticality_System;
with Core_Execution_Modes;

package body CPU_Budget_Monitor is

   procedure CPU_BE_Detected (E : in out Timing_Event) is
      use System.BB.Threads;
      use System.BB.Threads.Queues;
      use Mixed_Criticality_System;
      use Core_Execution_Modes;
      use System.BB.Board_Support.Multiprocessors;
      pragma Unreferenced (E);
      CPU_Id : constant System.Multiprocessors.CPU := Current_CPU;
      Self_Id : constant Thread_Id := Running_Thread;
      Task_Exceeded : constant Integer := Self_Id.Base_Priority;
      pragma Warnings (Off);
   begin
      System.BB.Protection.Enter_Kernel;
      Ada.Text_IO.Put ("CPU_" & System.Multiprocessors.CPU'Image (CPU_Id)
                              & ": task " & Integer'Image (Task_Exceeded));

      if Get_Core_Mode (CPU_Id) = LOW then
         if Self_Id.Criticality_Level = HIGH then
            Ada.Text_IO.Put_Line (" HI-CRIT CPU_Budget_Exceeded DETECTED.");
            loop
               null;
            end loop;
            Set_Core_Mode (HIGH, CPU_Id);
            Enter_In_HI_Crit_Mode;
         else
            loop
               null;
            end loop;
            Ada.Text_IO.Put_Line ("");
            Ada.Text_IO.Put_Line
                     ("-------------------------------------------------");
            Ada.Text_IO.Put_Line
                     ("--  LO-crit task exceeding its LO-crit budget  --");
            Ada.Text_IO.Put_Line
                     ("--        !!!  INVALID EXPERIMENTS  !!!        --");
            Ada.Text_IO.Put_Line
                     ("-------------------------------------------------");
         end if;
      else
            Ada.Text_IO.Put_Line ("");
            Ada.Text_IO.Put_Line
               ("----------------------------------------------------------");
            Ada.Text_IO.Put_Line
               ("--        A task has exceeded its current budget        --");
            Ada.Text_IO.Put_Line
               ("--      Unpredictable overload during HI-crit mode      --");
            Ada.Text_IO.Put_Line
               ("--             !!!  INVALID EXPERIMENTS  !!!            --");
            Ada.Text_IO.Put_Line
               ("----------------------------------------------------------");
      end if;

      System.BB.Threads.Queues.Print_Queues;
      System.BB.Protection.Leave_Kernel;
      --  Ada.Text_IO.Put_Line ("BE HANDLED");
   end CPU_BE_Detected;

   procedure Start_Monitor (For_Time : System.BB.Time.Time_Span) is
      use Real_Time_No_Elab;
      use System.BB.Board_Support.Multiprocessors;
      CPU_Id : constant System.Multiprocessors.CPU := Current_CPU;
   begin
      Set_Handler
            (Event =>
                BE_Happened (CPU_Id),
            At_Time =>
                For_Time + Real_Time_No_Elab.Clock,
            Handler =>
                CPU_BE_Detected'Access);

      --  Ada.Text_IO.Put_Line ("CPU_" & System.Multiprocessors.CPU'Image
      --                   (CPU_Id) & ": Budget monitoring has been STARTED.");

   end Start_Monitor;

   procedure Clear_Monitor (Cancelled : out Boolean) is
      use System.BB.Board_Support.Multiprocessors;
      CPU_Id : constant System.Multiprocessors.CPU := Current_CPU;
   begin
      Cancel_Handler
            (BE_Happened (Current_CPU), Cancelled);

      --  Ada.Text_IO.Put_Line ("CPU_" & System.Multiprocessors.CPU'Image
      --                   (CPU_Id) & ": Budget monitoring has been CLEARED.");
   end Clear_Monitor;

end CPU_Budget_Monitor;
