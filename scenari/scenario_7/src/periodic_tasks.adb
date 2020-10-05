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
   --  type Low_Crit_First_CPU  --
   -------------------------------

   task body Low_Crit_First_CPU is
      Next_Period : Ada.Real_Time.Time := Ada.Real_Time.Time_First + Ada.Real_Time.Microseconds (Experiments_Data.Delay_Time);
      Period_To_Add : constant Ada.Real_Time.Time_Span := Ada.Real_Time.Microseconds (Period);
   begin

      STPO.Initialize_LO_Crit_Task
            (STPO.Self, System.BB.Time.Microseconds (Low_Critical_Budget),
            Period, Is_Migrable);

      loop
         delay until Next_Period;
         Ada.Text_IO.Put_Line (Integer'Image (Pri) & " = " & Integer'Image (System.BB.Threads.Queues.Running_Thread.Base_Priority));
         System.BB.Threads.Queues.Print_Queues;
         Production_Workload.Small_Whetstone (Workload);
         Next_Period := Next_Period + Period_To_Add;
      end loop;
   end Low_Crit_First_CPU;

   -------------------------------
   --  type High_Crit_First_CPU  --
   -------------------------------

   task body High_Crit_First_CPU is
      Next_Period : Ada.Real_Time.Time := Ada.Real_Time.Time_First + Ada.Real_Time.Microseconds (Experiments_Data.Delay_Time);
      Period_To_Add : constant Ada.Real_Time.Time_Span := Ada.Real_Time.Microseconds (Period);
   begin

      STPO.Initialize_HI_Crit_Task
            (STPO.Self, System.BB.Time.Microseconds (Low_Critical_Budget),
            System.BB.Time.Microseconds (High_Critical_Budget), Period);

      loop
         delay until Next_Period;
         Ada.Text_IO.Put_Line (Integer'Image (Pri) & " = " & Integer'Image (System.BB.Threads.Queues.Running_Thread.Base_Priority));
         System.BB.Threads.Queues.Print_Queues;
         Production_Workload.Small_Whetstone (Workload);
         Next_Period := Next_Period + Period_To_Add;
      end loop;
   end High_Crit_First_CPU;

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

   HC1 : High_Crit_First_CPU
      (Pri => 40, Low_Critical_Budget => 200_000,
      High_Critical_Budget => 1_500_000, Workload => 100_000, Period => 1_000_000);

   HC2 : High_Crit_First_CPU
      (Pri => 30, Low_Critical_Budget => 200_000,
      High_Critical_Budget => 1_500_000, Workload => 100_000, Period => 3_000_000);

   LC1 : Low_Crit_First_CPU
      (Pri => 20, Low_Critical_Budget => 200_000,
      Is_Migrable => True, Workload => 1, Period => 200_000);

   LC2 : Low_Crit_First_CPU
      (Pri => 10, Low_Critical_Budget => 200_000,
      Is_Migrable => True, Workload => 1, Period => 200_000);

end Periodic_Tasks;