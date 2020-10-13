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

      --  Log change mode.
      if Mode_Cores (CPU_Id) /= Core_Mode then
         if Mode_Cores (CPU_Id) = LOW then
            CPU_Log_Table (CPU_Id).Low_To_High
                                    := CPU_Log_Table (CPU_Id).Low_To_High + 1;
         else
            CPU_Log_Table (CPU_Id).High_To_Low
                                    := CPU_Log_Table (CPU_Id).High_To_Low + 1;
         end if;
      end if;

      Mode_Cores (CPU_Id) := Core_Mode;
   end Set_Core_Mode;

   function Get_Core_Mode (CPU_Id : CPU) return Mode is
   begin
      --  Interrupts must be disabled

      return Mode_Cores (CPU_Id);
   end Get_Core_Mode;

   procedure Print_CPUs_Log is
      use System.BB.Time;
   begin
      for CPU_Id in CPU_Log_Table'Range loop
         Ada.Text_IO.Put_Line ("--  CPU" & CPU_Range'Image (CPU_Id) & "  --");
         Ada.Text_IO.Put_Line ("LOW => HIGH "
            & Natural'Image (CPU_Log_Table (CPU_Id).Low_To_High) & " Times");

         Ada.Text_IO.Put_Line ("HIGH => LOW "
            & Natural'Image (CPU_Log_Table (CPU_Id).High_To_Low) & " Times");

         Ada.Text_IO.Put_Line ("Idle time: " &
            Duration'Image (To_Duration (CPU_Log_Table (CPU_Id).Idle_Time)));

         Ada.Text_IO.Put_Line ("");
      end loop;
   end Print_CPUs_Log;

end Core_Execution_Modes;
