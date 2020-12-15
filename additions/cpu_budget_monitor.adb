pragma Warnings (Off);
with Ada.Text_IO;
with System.BB.Execution_Time;
pragma Warnings (On);

with System.BB.Protection;
with System.BB.Board_Support;
with System.BB.Threads.Queues;
with Mixed_Criticality_System;
with Core_Execution_Modes;
with Experiment_Info;
with Initial_Delay;

package body CPU_Budget_Monitor is

   procedure CPU_BE_Detected (E : in out Timing_Event) is
      use System.BB.Threads;
      use System.BB.Threads.Queues;
      use Mixed_Criticality_System;
      use Core_Execution_Modes;
      use System.BB.Board_Support.Multiprocessors;
      use System.Multiprocessors;
      --  use System.BB.Time;
      pragma Unreferenced (E);
      CPU_Id : constant CPU := Current_CPU;
      Self_Id : constant Thread_Id := Running_Thread;
      Task_Id : Integer;
      Task_Exceeded : constant System.Priority :=
                    Self_Id.Data_Concerning_Migration.Id;
      Cancelled : Boolean;
   begin
      System.BB.Protection.Enter_Kernel;

      --  Log CPU_Budget_Exceeded
      Self_Id.Log_Table.Times_BE := Self_Id.Log_Table.Times_BE + 1;
      --  Ada.Text_IO.Put ("CPU_" & System.Multiprocessors.CPU'Image (CPU_Id)
      --                         & ": task " & Integer'Image (Task_Exceeded));

      --  Log that CPU_Budget_Exceeded has been happened on target CPU
      if Self_Id.Active_CPU /= Self_Id.Base_CPU then
         Task_Id := Self_Id.Data_Concerning_Migration.Id;
         Executions (Task_Id).BE_On_Target_Core :=
           Executions (Task_Id).BE_On_Target_Core + 1;
      end if;

      --  Log that CPU_Budget_Exceeded has been happened after migration(s).
      if Executions (Task_Id).Migration_Happened_Current_Job_Release then
         Executions (Task_Id).BE_After_Migration :=
                              Executions (Task_Id).BE_After_Migration + 1;
      end if;

      if Get_Core_Mode (CPU_Id) = LOW then
         if Self_Id.Criticality_Level = HIGH then
            Clear_Monitor (Cancelled);
            Self_Id.Active_Budget := 0;
            --  Ada.Text_IO.Put_Line
            --   (" HI-CRIT CPU_Budget_Exceeded DETECTED.");
            Set_Core_Mode (HIGH, CPU_Id);
            Enter_In_HI_Crit_Mode;

            Start_Monitor (Self_Id.Active_Budget);
         else
            Experiment_Is_Not_Valid := True;
            Guilty_Task := Task_Exceeded;
            Set_Parameters_Referee
               (False, Experiment_Is_Not_Valid, False);
            --  Ada.Text_IO.Put_Line ("");
            --  Ada.Text_IO.Put_Line ("CPU_"
            --               & System.Multiprocessors.CPU'Image (CPU_Id)
            --            & ": GUILTY task " & Integer'Image (Task_Exceeded));

            --  Ada.Text_IO.Put_Line
            --         ("-------------------------------------------------");
            --  Ada.Text_IO.Put_Line
            --         ("--  LO-crit task exceeding its LO-crit budget  --");
            --  Ada.Text_IO.Put_Line
            --         ("--        !!!  INVALID EXPERIMENTS  !!!        --");
            --  Ada.Text_IO.Put_Line
            --         ("-------------------------------------------------");
            --  loop
            --     null;
            --  end loop;
         end if;
      else  --  Get_Core_Mode (CPU_Id) is HIGH
            Experiment_Is_Not_Valid := True;
            Guilty_Task := Task_Exceeded;
            Set_Parameters_Referee
                  (False, Experiment_Is_Not_Valid, False);
            --  Ada.Text_IO.Put_Line ("");
            --  Ada.Text_IO.Put_Line ("CPU_"
            --               & System.Multiprocessors.CPU'Image (CPU_Id)
            --             & ": GUILTY task " & Integer'Image (Task_Exceeded));
            --  Ada.Text_IO.Put_Line
            --  ("----------------------------------------------------------");
            --  Ada.Text_IO.Put_Line
            --  ("--        A task has exceeded its current budget        --");
            --  Ada.Text_IO.Put_Line
            --  ("--      Unpredictable overload during HI-crit mode      --");
            --  Ada.Text_IO.Put_Line
            --  ("--             !!!  INVALID EXPERIMENTS  !!!            --");
            --  Ada.Text_IO.Put_Line
            --  ("----------------------------------------------------------");
            --  loop
            --     null;
            --  end loop;
      end if;

      System.BB.Protection.Leave_Kernel;
      --  Ada.Text_IO.Put_Line ("BE HANDLED");
   end CPU_BE_Detected;

   --  return True iff we have detected the passage of the hyperperiod
   --  for at least once. It is useful in order to stop logging CPU's Idle_Time
   --  as soon as its hyperperiod expires.
   function Hyperperiod_Not_Yet_Passed
     (CPU_Id : System.Multiprocessors.CPU) return Boolean;

   procedure Start_Monitor (For_Time : System.BB.Time.Time_Span) is
      use Real_Time_No_Elab;
      --  use System.BB.Board_Support.Multiprocessors;
      use System.BB.Threads;
      use System.BB.Threads.Queues;
      use Core_Execution_Modes;
      use System.Multiprocessors;
      Self_Id : constant Thread_Id := Running_Thread;
      CPU_Id : constant CPU := Self_Id.Active_CPU;
      --  Task_Exceeded : constant System.Priority := Self_Id.Base_Priority;
   begin
      Set_Handler
            (Event =>
                BE_Happened (CPU_Id),
            At_Time =>
                For_Time + Real_Time_No_Elab.Clock,
            Handler =>
                CPU_BE_Detected'Access);
      --  Ada.Text_IO.Put_Line (Integer'Image (Task_Exceeded) & " armed with"
      --                    & Duration'Image (To_Duration (For_Time)));

      --  Log that CPU_Id is no longer idle.
      if CPU_Log_Table (CPU_Id).Is_Idle and Hyperperiod_Not_Yet_Passed (CPU_Id)
      then
         CPU_Log_Table (CPU_Id).Is_Idle := False;

         CPU_Log_Table (CPU_Id).Idle_Time :=
                     CPU_Log_Table (CPU_Id).Idle_Time +
            (Clock - CPU_Log_Table (CPU_Id).Last_Time_Idle);
      end if;

      --  Log that thread is (again) on this CPU
      if CPU_Id = CPU'First then
         Executions (Self_Id.Data_Concerning_Migration.Id).Times_On_First_CPU
           := Executions (Self_Id.Data_Concerning_Migration.Id).
                                                  Times_On_First_CPU + 1;
      else
         Executions (Self_Id.Data_Concerning_Migration.Id).Times_On_Second_CPU
           := Executions (Self_Id.Data_Concerning_Migration.Id).
                                                  Times_On_Second_CPU + 1;
      end if;

      Self_Id.T_Start := System.BB.Time.Clock;
   end Start_Monitor;

   procedure Clear_Monitor (Cancelled : out Boolean) is
      --  use System.BB.Board_Support.Multiprocessors;
      use System.BB.Threads;
      use System.BB.Time;
      use System.BB.Threads.Queues;
      Self_Id : constant Thread_Id := Running_Thread;
      CPU_Id : constant System.Multiprocessors.CPU :=
                                                      Self_Id.Active_CPU;
   begin
      Self_Id.T_Clear := System.BB.Time.Clock;

      Cancel_Handler (BE_Happened (CPU_Id), Cancelled);

      if Self_Id.Is_Monitored and Self_Id.State = Runnable then
         --  Ada.Text_IO.Put (Integer'Image (Self_Id.Base_Priority)
         --  & " consumed" & Duration'Image
         --  (System.BB.Time.To_Duration (Self_Id.Active_Budget)) & " => ");

         Self_Id.Active_Budget :=
                  Self_Id.Active_Budget - (Self_Id.T_Clear - Self_Id.T_Start);

         --  Ada.Text_IO.Put_Line (Duration'Image (System.BB.Time.To_Duration
         --                                    (Self_Id.Active_Budget)));
      end if;
   end Clear_Monitor;

   Hyperperiod_Passed_First_Time : array (System.Multiprocessors.CPU)
                                               of Boolean := (others => False);

   function Hyperperiod_Not_Yet_Passed
     (CPU_Id : System.Multiprocessors.CPU) return Boolean is
      use Real_Time_No_Elab;
      Hyperperiod : Real_Time_No_Elab.Time_Span;
      Hyperperiod_With_Delay : Real_Time_No_Elab.Time_Span;
      Absolute_Hyperperiod : Real_Time_No_Elab.Time;
   begin
      Hyperperiod := Microseconds
        (Experiment_Info.Get_Parameters.Experiment_Hyperperiods (CPU_Id));

      Hyperperiod_With_Delay := Real_Time_No_Elab."+"
        (Microseconds (Initial_Delay.Delay_Time), Hyperperiod);

      Absolute_Hyperperiod := Real_Time_No_Elab."+"
        (Real_Time_No_Elab.Time_First, Hyperperiod_With_Delay);

      if (not Hyperperiod_Passed_First_Time (CPU_Id))
        and
         Real_Time_No_Elab.">=" (Real_Time_No_Elab.Clock, Absolute_Hyperperiod)
      then
         Hyperperiod_Passed_First_Time (CPU_Id) := True;
      end if;

      return not Hyperperiod_Passed_First_Time (CPU_Id);
   end Hyperperiod_Not_Yet_Passed;

end CPU_Budget_Monitor;
