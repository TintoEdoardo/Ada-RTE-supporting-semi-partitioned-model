with Ada.Real_Time; use Ada.Real_Time;

package body Periodic_Tasks is

   task body Periodic_First_CPU is
      Next_Period : Ada.Real_Time.Time;
      Period_To_Add : constant Ada.Real_Time.Time_Span := Ada.Real_Time.Microseconds (Period);
   begin
      Next_Period := Ada.Real_Time.Clock + Period_To_Add;
      loop
         delay until Next_Period;
         Next_Period := Next_Period + Period_To_Add;
      end loop;
   end Periodic_First_CPU;

   procedure Init is
   begin
      loop
         null;
      end loop;
   end Init;

   P1 : Periodic_First_CPU (Pri => 10, Period => 1_000_000);
   
end Periodic_Tasks;