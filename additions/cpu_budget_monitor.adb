pragma Warnings (Off);
with Ada.Text_IO;
pragma Warnings (On);

with System.BB.Board_Support;

package body CPU_Budget_Monitor is
   --  package BOSUMU renames System.BB.Board_Support.Multiprocessors;

   procedure CPU_BE_Detected (E : in out Timing_Event) is
      pragma Unreferenced (E);
   begin
      Ada.Text_IO.Put_Line ("CPU_Budget_Exceeded DETECTED.");
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

      Ada.Text_IO.Put_Line ("Budget monitoritng has been CLEARED.");
   end Clear_Monitor;

end CPU_Budget_Monitor;
