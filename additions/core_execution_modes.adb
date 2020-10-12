pragma Warnings (Off);
with Ada.Text_IO;
pragma Warnings (On);

with System.BB.Board_Support;

package body Core_Execution_Modes is

   procedure Set_Core_Mode (Core_Mode : Mode; CPU_Id : CPU) is
      use System.BB.Board_Support.Multiprocessors;
   begin
      --  Interrupts must be disabled
      --  A CPU can only change its own criticality mode
      pragma Assert (CPU_Id = Current_CPU);

      if CPU_Id = 1 then
         Ada.Text_IO.Put_Line ("CPU_" & CPU'Image (CPU_Id) & ": "
           & Mode'Image (Mode_Cores (CPU_Id)) & " => "
           & Mode'Image (Core_Mode));
      end if;
      Mode_Cores (CPU_Id) := Core_Mode;
   end Set_Core_Mode;

   function Get_Core_Mode (CPU_Id : CPU) return Mode is
   begin
      --  Interrupts must be disabled

      return Mode_Cores (CPU_Id);
   end Get_Core_Mode;

end Core_Execution_Modes;
