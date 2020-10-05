with System;
with Mixed_Criticality_System;
use Mixed_Criticality_System;

pragma Warnings (Off);
with System.BB.Time;
pragma Warnings (On);

package Periodic_Tasks is

  task type Periodic_First_CPU
   (Pri               : System.Priority;
    Budget            : Natural;
    Is_Migrable       : Boolean;
    Criticality_Level : Criticality;
    Workload          : Positive;
    Period            : Positive) with CPU => 1
  is
    pragma Priority (Pri);
  end Periodic_First_CPU;

  --  Periodic task that is not subject to budget monitoring
  task type Periodic_Not_Monitored
   (Pri      : System.Priority;
    Period   : Positive) with CPU => 1
  is
    pragma Priority (Pri);
  end Periodic_Not_Monitored;

  --  Periodic task that, sooner or later, will force a CPU_Budget_Exceeded
  task type BE_First_CPU
   (Pri               : System.Priority;
    Budget            : Natural;
    Is_Migrable       : Boolean;
    Criticality_Level : Criticality;
    Workload          : Positive;
    Period            : Positive) with CPU => 1
  is
    pragma Priority (Pri);
  end BE_First_CPU;

  procedure Init;
  pragma No_Return (Init);

private

  protected Initialization_Done is
    procedure Inform_Monitor (Budget : System.BB.Time.Time_Span);
  end Initialization_Done;

end Periodic_Tasks;