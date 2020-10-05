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
   begin
      System.BB.Protection.Enter_Kernel;
      Ada.Text_IO.Put (Integer'Image (Task_Exceeded));

      if Get_Core_Mode (CPU_Id) = LOW then
         if Self_Id.Criticality_Level = HIGH then
            Ada.Text_IO.Put_Line (" HI-CRIT CPU_Budget_Exceeded DETECTED.");

            Set_Core_Mode (HIGH, CPU_Id);
            Discard_Tasks;
         else
            Ada.Text_IO.Put_Line (" LO-CRIT CPU_Budget_Exceeded DETECTED.");
         end if;
      end if;

      --  Self_Id.State := Discarded;
      --  Extract (Self_Id);
      --  Insert_Discarded (Self_Id);
      System.BB.Threads.Queues.Print_Queues;
      System.BB.Protection.Leave_Kernel;
      Ada.Text_IO.Put_Line ("BE HANDLED");
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

      Ada.Text_IO.Put_Line ("Budget monitoring has been STARTED.");

   end Start_Monitor;

   procedure Clear_Monitor (Cancelled : out Boolean) is
      use System.BB.Board_Support.Multiprocessors;
   begin
      Cancel_Handler
            (BE_Happened (Current_CPU), Cancelled);

      Ada.Text_IO.Put_Line ("Budget monitoring has been CLEARED");
   end Clear_Monitor;

end CPU_Budget_Monitor;
