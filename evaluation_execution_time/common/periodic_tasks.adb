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

with Single_Execution_Data;
with Production_Workload;
with Initial_Delay;

package body Periodic_Tasks is

   Max_Difference : Ada.Real_Time.Time_Span := Ada.Real_Time.Time_Span_First;
   Min_Difference : Ada.Real_Time.Time_Span := Ada.Real_Time.Time_Span_Last;

   package STPO renames System.Task_Primitives.Operations;

   function Get_Longest_Hyperperiod return Natural is
   begin
      if Single_Execution_Data.Experiment_Hyperperiods (CPU'First) > Single_Execution_Data.Experiment_Hyperperiods (CPU'Last) then
         return Single_Execution_Data.Experiment_Hyperperiods (CPU'First);
      end if;

      return Single_Execution_Data.Experiment_Hyperperiods (CPU'Last);
   end Get_Longest_Hyperperiod;

   ---------------------
   --  type Low_Crit  --
   ---------------------

   task body Low_Crit is
      Next_Period : Ada.Real_Time.Time := Ada.Real_Time.Time_First + Ada.Real_Time.Microseconds (Initial_Delay.Delay_Time);
      Period_To_Add : constant Ada.Real_Time.Time_Span := Ada.Real_Time.Microseconds (Period);
   begin
      STPO.Initialize_LO_Crit_Task (STPO.Self, Id, Hosting_Migrating_Tasks_Priority, On_Target_Core_Priority,
                                    System.BB.Time.Microseconds (Low_Critical_Budget), Period, Reduced_Deadline, Is_Migrable);
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
      Next_Period : Ada.Real_Time.Time := Ada.Real_Time.Time_First + Ada.Real_Time.Microseconds (Initial_Delay.Delay_Time);
      Period_To_Add : constant Ada.Real_Time.Time_Span := Ada.Real_Time.Microseconds (Period);
      I : Natural := 0;
      Start : Ada.Real_Time.Time;
      Finish : Ada.Real_Time.Time;
      Difference : Ada.Real_Time.Time_Span;
   begin
      STPO.Initialize_HI_Crit_Task
         (STPO.Self, Id, Hosting_Migrating_Tasks_Priority, System.BB.Time.Microseconds (Low_Critical_Budget), System.BB.Time.Microseconds (High_Critical_Budget), Period);
      Ada.Text_IO.Put_Line ("Workload: " & Positive'Image(Workload));
      
      loop
         delay until Next_Period;

         Start := Ada.Real_Time.Clock;
         Production_Workload.Small_Whetstone (Workload);
         Finish := Ada.Real_Time.Clock;

         Difference := Finish - Start;

         if Difference > Max_Difference then
            Max_Difference := Difference;
         end if;

         if Difference < Min_Difference then
            Min_Difference := Difference;
         end if;

         --  Ada.Text_IO.Put_Line (Duration'Image (To_Duration (Difference)));

         I := I + 1;
         
         Next_Period := Next_Period + Period_To_Add;
      end loop;
   end High_Crit;

   ------------
   --  Init  --
   ------------

   procedure Init is
      Next_Period   : constant Ada.Real_Time.Time := Ada.Real_Time.Time_First + Ada.Real_Time.Microseconds (Initial_Delay.Delay_Time);
      Period_To_Add : constant Ada.Real_Time.Time_Span := Ada.Real_Time.Microseconds (Get_Longest_Hyperperiod);
   begin
      delay until Next_Period + Period_To_Add;
      --  Ada.Text_IO.Put_Line ("----------------------");
      --  Ada.Text_IO.Put_Line ("--  END EXPERIMENT  --");
      --  Ada.Text_IO.Put_Line ("----------------------");

      --  System.BB.Threads.Queues.Print_Queues;
      Ada.Text_IO.Put_Line ("Max: " & Duration'Image (To_Duration (Max_Difference)));
      Ada.Text_IO.Put_Line ("Min: " & Duration'Image (To_Duration (Min_Difference)));
      Ada.Text_IO.Put_Line ("Gap: " & Duration'Image (To_Duration (Max_Difference - Min_Difference)));

      loop
         null;
      end loop;
   end Init;

   ----------------------------
   --  End_Task_Second_Core  --
   ----------------------------
   
   --  This task stucks second core's execution when current experiment should stops.
   task End_Task_Second_Core with 
         Priority => System.Priority'Last - 1,
         CPU      => CPU'Last;

   task body End_Task_Second_Core is
      Next_Period : constant Ada.Real_Time.Time := Ada.Real_Time.Time_First + Ada.Real_Time.Microseconds (Initial_Delay.Delay_Time);
      Period_To_Add : constant Ada.Real_Time.Time_Span := Ada.Real_Time.Microseconds (Get_Longest_Hyperperiod);
   begin
      delay until Next_Period + Period_To_Add;

      loop
         null;
      end loop;
   end End_Task_Second_Core;

end Periodic_Tasks;