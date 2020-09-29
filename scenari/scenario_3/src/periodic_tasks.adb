with Ada.Real_Time; use Ada.Real_Time;
with Ada.Text_IO;

pragma Warnings (Off);
with System.BB.Time;
use System.BB.Time;
with System.Task_Primitives.Operations;
pragma Warnings (On);

with CPU_Budget_Monitor;
with Production_Workload;

package body Periodic_Tasks is

   package STPO renames System.Task_Primitives.Operations;

   -------------------------------
   --  type Periodic_First_CPU  --
   -------------------------------

   task body Periodic_First_CPU is
      Next_Period : Ada.Real_Time.Time;
      Period_To_Add : constant Ada.Real_Time.Time_Span := Ada.Real_Time.Microseconds (Period);
   begin
      STPO.Set_Budget (STPO.Self, System.BB.Time.Microseconds (Budget));
      Next_Period := Ada.Real_Time.Clock + Period_To_Add;

      if System.BB.Time.Microseconds (Budget) = STPO.Get_Thread_Id (STPO.Self).Budget then
         Ada.Text_IO.Put_Line ("Budget correctly setted");
      end if;

      Initialization_Done.Inform_Monitor (System.BB.Time.Microseconds (Budget));

      loop
         Production_Workload.Small_Whetstone (500_000);
         delay until Next_Period;
         Production_Workload.Small_Whetstone (Workload);
         Production_Workload.Small_Whetstone (500_000);
         Next_Period := Next_Period + Period_To_Add;
      end loop;
   end Periodic_First_CPU;

   -----------------------------------
   --  type Periodic_Not_Monitored  --
   -----------------------------------

   task body Periodic_Not_Monitored is
      Next_Period : Ada.Real_Time.Time;
      Period_To_Add : constant Ada.Real_Time.Time_Span := Ada.Real_Time.Microseconds (Period);
   begin
      Next_Period := Ada.Real_Time.Clock + Period_To_Add;
      loop
         delay until Next_Period;
         --  Production_Workload.Small_Whetstone (500_000);
         Next_Period := Next_Period + Period_To_Add;
      end loop;
   end Periodic_Not_Monitored;

   ------------
   --  Init  --
   ------------

   procedure Init is
   begin
      loop
         Ada.Text_IO.Put_Line ("Init");
      end loop;
   end Init;

   -------------------------------------
   --  protected Initialization_Done  --
   -------------------------------------

   protected body Initialization_Done is
      procedure Inform_Monitor (Budget : System.BB.Time.Time_Span) is
      begin
         CPU_Budget_Monitor.Start_Monitor (Budget);
      end Inform_Monitor;
   end Initialization_Done;

   P1    : Periodic_First_CPU (Pri => 10, Budget => 200_000, Workload => 1, Period => 400_000);
   --  PNM_1 : Periodic_Not_Monitored (Pri => 11, Period => 10_200_000);
   
end Periodic_Tasks;