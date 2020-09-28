pragma Warnings (Off);
with Ada.Text_IO;
pragma Warnings (On);

package body CPU_Budget_Monitor is

   procedure CPU_BE_Detected (E : in out Timing_Event) is
      pragma Unreferenced (E);
   begin
      Ada.Text_IO.Put_Line ("CPU_Budget_Exceeded DETECTED.");
   end CPU_BE_Detected;

   procedure Start_Monitor (For_Time : System.BB.Time.Time_Span) is
      pragma Unreferenced (For_Time);
   begin
      Ada.Text_IO.Put_Line ("Budget monitoring has been STARTED.");
   end Start_Monitor;

   procedure Clear_Monitor (Cancelled : out Boolean) is
      pragma Unreferenced (Cancelled);
   begin
      Ada.Text_IO.Put_Line ("Budget monitoritng has been CLEARED.");
   end Clear_Monitor;

end CPU_Budget_Monitor;
