with System;

pragma Warnings (Off);
with System.BB.Time;
pragma Warnings (On);

package Periodic_Tasks is

  --  Periodic LO-CRIT task
  task type Low_Crit_First_CPU
      (Pri                 : System.Priority;
      Low_Critical_Budget : Natural;
      Is_Migrable         : Boolean;
      Workload            : Positive;
      Period              : Positive) with CPU => 1
  is
      pragma Priority (Pri);
  end Low_Crit_First_CPU;

  --  Periodic HI-CRIT task
  task type High_Crit_First_CPU
      (Pri                  : System.Priority;
      Low_Critical_Budget  : Natural;
      High_Critical_Budget : Natural;
      Workload             : Positive;
      Period               : Positive) with CPU => 1
  is
      pragma Priority (Pri);
  end High_Crit_First_CPU;

  --  Periodic task that is not subject to budget monitoring
  task type Periodic_Not_Monitored
   (Pri      : System.Priority;
    Period   : Positive) with CPU => 1
  is
    pragma Priority (Pri);
  end Periodic_Not_Monitored;

  procedure Init;
  pragma No_Return (Init);

private

  protected Initialization_Done is
    procedure Inform_Monitor (Budget : System.BB.Time.Time_Span);
  end Initialization_Done;

end Periodic_Tasks;