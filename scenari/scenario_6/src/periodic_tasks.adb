with Ada.Real_Time; use Ada.Real_Time;
with Ada.Text_IO;

pragma Warnings (Off);
with System.BB.Time;
use System.BB.Time;
with System.Task_Primitives.Operations;
with System.BB.Threads.Queues;
pragma Warnings (On);

with CPU_Budget_Monitor;
with Production_Workload;
with Experiments_Data;

package body Periodic_Tasks is

   package STPO renames System.Task_Primitives.Operations;

   -------------------------------
   --  type Periodic_First_CPU  --
   -------------------------------

   task body Periodic_First_CPU is
      Next_Period : Ada.Real_Time.Time := Ada.Real_Time.Time_First + Ada.Real_Time.Microseconds (Experiments_Data.Delay_Time);
      Period_To_Add : constant Ada.Real_Time.Time_Span := Ada.Real_Time.Microseconds (Period);
   begin
      STPO.Set_Budget (STPO.Self, System.BB.Time.Microseconds (Budget), Period);

      if Criticality_Level = LOW then
         STPO.Initialize_LO_Crit_Task (STPO.Self, Is_Migrable);
      else
         STPO.Initialize_HI_Crit_Task (STPO.Self);
      end if;

      if System.BB.Time.Microseconds (Budget) = STPO.Get_Thread_Id (STPO.Self).Budget then
         Ada.Text_IO.Put_Line ("Budget correctly setted");
      end if;

      --  Next_Period := Ada.Real_Time.Clock + Period_To_Add;

      --  Initialization_Done.Inform_Monitor (System.BB.Time.Microseconds (Budget));

      loop
         delay until Next_Period;
         Ada.Text_IO.Put_Line (Integer'Image (Pri) & " = " & Integer'Image (System.BB.Threads.Queues.Running_Thread.Base_Priority));
         System.BB.Threads.Queues.Print_Queues;
         Production_Workload.Small_Whetstone (Workload);
         Next_Period := Next_Period + Period_To_Add;
      end loop;
   end Periodic_First_CPU;

   -----------------------------------
   --  type Periodic_Not_Monitored  --
   -----------------------------------

   task body Periodic_Not_Monitored is
      Next_Period : Ada.Real_Time.Time := Ada.Real_Time.Time_First + Ada.Real_Time.Microseconds (Experiments_Data.Delay_Time);
      Period_To_Add : constant Ada.Real_Time.Time_Span := Ada.Real_Time.Microseconds (Period);
   begin
      --  Next_Period := Ada.Real_Time.Clock + Period_To_Add;
      loop
         delay until Next_Period;
         Next_Period := Next_Period + Period_To_Add;
      end loop;
   end Periodic_Not_Monitored;

   -------------------------
   --  type BE_First_CPU  --
   -------------------------

   task body BE_First_CPU is
      Next_Period : Ada.Real_Time.Time := Ada.Real_Time.Time_First + Ada.Real_Time.Microseconds (Experiments_Data.Delay_Time);
      Period_To_Add : constant Ada.Real_Time.Time_Span := Ada.Real_Time.Microseconds (Period);
      I : Natural := 1;
   begin
      STPO.Set_Budget (STPO.Self, System.BB.Time.Microseconds (Budget), Period);

      if Criticality_Level = LOW then
         STPO.Initialize_LO_Crit_Task (STPO.Self, Is_Migrable);
      else
         STPO.Initialize_HI_Crit_Task (STPO.Self);
      end if;

      --  Next_Period := Ada.Real_Time.Clock + Period_To_Add;

      --  Initialization_Done.Inform_Monitor (System.BB.Time.Microseconds (Budget));
      loop
         delay until Next_Period;
         Ada.Text_IO.Put_Line (Integer'Image (Pri) & " = " & Integer'Image (System.BB.Threads.Queues.Running_Thread.Base_Priority));
         System.BB.Threads.Queues.Print_Queues;
         if I rem 2 = 0 then
            Production_Workload.Small_Whetstone (Workload);
         end if;

         I := I + 1;

         Next_Period := Next_Period + Period_To_Add;
      end loop;
   end BE_First_CPU;

   ------------
   --  Init  --
   ------------

   procedure Init is
      Next_Period : Ada.Real_Time.Time := Ada.Real_Time.Time_First + Ada.Real_Time.Microseconds (Experiments_Data.Delay_Time);
      Period_To_Add : constant Ada.Real_Time.Time_Span := Ada.Real_Time.Microseconds (6_000_000);
   begin
      --  Next_Period := Ada.Real_Time.Clock + Period_To_Add;
      loop
         delay until Next_Period + Period_To_Add;
         Ada.Text_IO.Put_Line ("----------------------");
         Ada.Text_IO.Put_Line ("--  END EXPERIMENT  --");
         Ada.Text_IO.Put_Line ("----------------------");
         Next_Period := Next_Period + Period_To_Add;
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

   ---------------------------------------------
   --  Tasks allocation => start experiments  --
   ---------------------------------------------

BE1 : BE_First_CPU
    (Pri => 40, Budget => 200_000, Criticality_Level => HIGH,
     Is_Migrable => False, Workload => 100_000, Period => 4_000_000);

P1 : Periodic_First_CPU
    (Pri => 10, Budget => 200_000, Criticality_Level => LOW,
     Is_Migrable => True, Workload => 1, Period => 200_000);

P2 : Periodic_First_CPU
    (Pri => 20, Budget => 200_000, Criticality_Level => LOW,
     Is_Migrable => True, Workload => 1, Period => 800_000);

P3 : BE_First_CPU
    (Pri => 30, Budget => 200_000, Criticality_Level => HIGH,
     Is_Migrable => False, Workload => 100_000, Period => 3_000_000);
   
end Periodic_Tasks;