pragma Profile (Ravenscar);

pragma Warnings (Off);
with System.Task_Primitives.Operations;
use System.Task_Primitives.Operations;

with System.BB.Time;
with System.BB.Threads;
pragma Warnings (On);

with Production_Workload;
with Ada.Real_Time;
with Budget_Overrun_Detection;

--  with Worker;
--  pragma Unreferenced (Worker);

with Two_Tasks;
pragma Unreferenced (Two_Tasks);

procedure Main is
 pragma Priority (0);
 use Ada.Real_Time;
 Next_Period : Ada.Real_Time.Time;
 Period : constant Ada.Real_Time.Time_Span := Ada.Real_Time.Milliseconds(2_000);
 LO_Budget : constant System.BB.Time.Time_Span := System.BB.Time.Milliseconds (50000);
 Overruned : Integer;

begin 
  --  Set_LO_Crit_Budget (Self, LO_Budget);

  Next_Period := Ada.Real_Time.Clock + Period;
  loop
    Production_Workload.Small_Whetstone (1);
    Overruned := Budget_Overrun_Detection.Overrun_Inspector.Get_Overruned
                                            (System.BB.Threads.Thread_Self.Base_Priority);
    delay until Next_Period;
    Next_Period := Next_Period + Period;
  end loop;
end Main;
