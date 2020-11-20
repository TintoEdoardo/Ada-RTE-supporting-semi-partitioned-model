with Ada.Real_Time; use Ada.Real_Time;
with Ada.Text_IO;

pragma Warnings (Off);
with System;
with System.BB.Time;
use System.BB.Time;
with System.Task_Primitives.Operations;
with System.BB.Threads.Queues;
with Core_Execution_Modes;
pragma Warnings (On);

with Production_Workload;
with Experiments_Data;

package body Periodic_Tasks is

   package STPO renames System.Task_Primitives.Operations;

   ---------------------
   --  type Low_Crit  --
   ---------------------

   task body Low_Crit is
      Next_Period : Ada.Real_Time.Time := Ada.Real_Time.Time_First + Ada.Real_Time.Microseconds (Experiments_Data.Delay_Time);
      Period_To_Add : constant Ada.Real_Time.Time_Span := Ada.Real_Time.Microseconds (Period);
   begin
      STPO.Initialize_LO_Crit_Task (STPO.Self, System.BB.Time.Microseconds (Low_Critical_Budget), Period, Is_Migrable);
      loop
         delay until Next_Period;
         --  Ada.Text_IO.Put_Line ("Task " & Integer'Image (System.BB.Threads.Queues.Running_Thread.Base_Priority) & " (belonging to CPU n."  & CPU_Range'Image (System.BB.Threads.Queues.Running_Thread.Base_CPU) & ") is executing on CPU n. " & CPU_Range'Image (System.BB.Threads.Queues.Running_Thread.Active_CPU));
         Production_Workload.Small_Whetstone (Workload);
         Next_Period := Next_Period + Period_To_Add;
      end loop;
   end Low_Crit;

   ----------------------
   --  type High_Crit  --
   ----------------------

   task body High_Crit is
      Next_Period : Ada.Real_Time.Time := Ada.Real_Time.Time_First + Ada.Real_Time.Microseconds (Experiments_Data.Delay_Time);
      Period_To_Add : constant Ada.Real_Time.Time_Span := Ada.Real_Time.Microseconds (Period);
      I : Natural := 0;
   begin
      STPO.Initialize_HI_Crit_Task (STPO.Self, System.BB.Time.Microseconds (Low_Critical_Budget), System.BB.Time.Microseconds (High_Critical_Budget), Period);

      loop
         delay until Next_Period;
         if I rem 2 = 0 then
            Production_Workload.Small_Whetstone (Workload);
         end if;
         I := I + 1;
         Next_Period := Next_Period + Period_To_Add;
      end loop;
   end High_Crit;

   ------------
   --  Init  --
   ------------

   procedure Init is
      Next_Period   : constant Ada.Real_Time.Time := Ada.Real_Time.Time_First + Ada.Real_Time.Microseconds (Experiments_Data.Delay_Time);
      Period_To_Add : constant Ada.Real_Time.Time_Span := Ada.Real_Time.Microseconds (Experiment_Hyperperiod);
   begin
      delay until Next_Period + Period_To_Add;
      Ada.Text_IO.Put_Line ("----------------------");
      Ada.Text_IO.Put_Line ("--  END EXPERIMENT  --");
      Ada.Text_IO.Put_Line ("----------------------");
      Core_Execution_Modes.Print_CPUs_Log;
      System.BB.Threads.Queues.Print_Tasks_Log;
      --  System.BB.Threads.Queues.Print_Queues;
      loop
         null;
      end loop;
   end Init;

   ---------------------------------------------
   --  Tasks allocation => start experiments  --
   ---------------------------------------------

   -------------
   --  CPU 1  --
   -------------

   HC_1_1 : High_Crit (Pri => 40, Low_Critical_Budget => 100_000, High_Critical_Budget => 1_000_000, Workload => 50_000, Period => 700_000, CPU_Id => CPU_A);
   
   HC_1_2 : High_Crit (Pri => 35, Low_Critical_Budget => 100_000, High_Critical_Budget => 1_000_000, Workload => 1_000, Period => 550_000, CPU_Id => CPU_A);

   --  LC_1_1 : Low_Crit (Pri => 20, Low_Critical_Budget => 300_000, Is_Migrable => False, Workload => 1, Period => 100_000, CPU_Id => CPU_A);

   --  LC_1_2 : Low_Crit (Pri => 5, Low_Critical_Budget => 1_440_000, Is_Migrable => True, Workload => 1, Period => 100_000, CPU_Id => CPU_A);

   --  LC_1_3 : Low_Crit (Pri => 24, Low_Critical_Budget => 1_440_000, Is_Migrable => True, Workload => 1, Period => 300_000, CPU_Id => CPU_A);

   --  LC_1_4 : Low_Crit (Pri => 23, Low_Critical_Budget => 1_440_000, Is_Migrable => True, Workload => 1, Period => 300_000, CPU_Id => CPU_A);

   -------------
   --  CPU 2  --
   -------------

   --  HC_2_1 : High_Crit (Pri => 30, Low_Critical_Budget => 100_000_000, High_Critical_Budget => 500_000_000, Workload => 90_000_000, Period => 100_000_000, CPU_Id => CPU_B);
   
   --  HC_2_2 : High_Crit (Pri => 35, Low_Critical_Budget => 100_000, High_Critical_Budget => 1_000_000, Workload => 1, Period => 1_550_000, CPU_Id => CPU_B);

   --  LC_2_1 : Low_Crit (Pri => 10, Low_Critical_Budget => 300_000, Is_Migrable => True, Workload => 1, Period => 1_000_000, CPU_Id => CPU_B);

   --  LC_2_2 : Low_Crit (Pri => 15, Low_Critical_Budget => 140_000, Is_Migrable => False, Workload => 1, Period => 160_000, CPU_Id => CPU_B);

   task End_Task_Second_Core with 
         Priority => System.Priority'Last - 1,
         CPU      => CPU'Last;

   task body End_Task_Second_Core is
      Next_Period : constant Ada.Real_Time.Time := Ada.Real_Time.Time_First + Ada.Real_Time.Microseconds (Experiments_Data.Delay_Time);
      Period_To_Add : constant Ada.Real_Time.Time_Span := Ada.Real_Time.Microseconds (Experiment_Hyperperiod);
   begin
      delay until Next_Period + Period_To_Add;
      loop
         null;
      end loop;
   end End_Task_Second_Core;

end Periodic_Tasks;