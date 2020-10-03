pragma Warnings (Off);

with System.Task_Primitives.Operations;
use System.Task_Primitives.Operations;

with System.BB.Time;
with System.BB.Threads;

pragma Warnings (On);

with Ada.Real_Time;
use Ada.Real_Time;

with Production_Workload;

with Budget_Overrun_Detection;

package body Two_Tasks is

    task body A is
        Kilo_Whets : constant Positive := 1;
        LO_Budget : constant System.BB.Time.Time_Span := System.BB.Time.Milliseconds (30000);
        Next_Period : Ada.Real_Time.Time;
        Period : constant Ada.Real_Time.Time_Span := Ada.Real_Time.Milliseconds(5000);
        Overruned : Integer;
    begin
        --  Set_LO_Crit_Budget (Self, LO_Budget);
        
        Next_Period := Ada.Real_Time.Clock + Period;

        loop
            Production_Workload.Small_Whetstone (50_000);
            --  Overruned := Budget_Overrun_Detection.Overrun_Inspector.Get_Overruned
                                            --  (System.BB.Threads.Thread_Self.Base_Priority);
            delay until Next_Period;
            Next_Period := Next_Period + Period;
        end loop;
    end A;

    task body B is
        Kilo_Whets : constant Positive := 1;
        LO_Budget : constant System.BB.Time.Time_Span := System.BB.Time.Milliseconds (10000);
        Next_Period : Ada.Real_Time.Time;
        Period : constant Ada.Real_Time.Time_Span := Ada.Real_Time.Milliseconds (7000);
        Overruned : Integer;
    begin
        --  Set_LO_Crit_Budget (Self, LO_Budget);
        
        Next_Period := Ada.Real_Time.Clock + Period;

        loop
            --  Overruned := Budget_Overrun_Detection.Overrun_Inspector.Get_Overruned
                                            --  (System.BB.Threads.Thread_Self.Base_Priority);
            Production_Workload.Small_Whetstone (50_000);
            delay until Next_Period;
            Next_Period := Next_Period + Period;
        end loop;
    end B;

end Two_Tasks;