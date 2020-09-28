with System;

pragma Warnings (Off);
with System.BB.Time;
pragma Warnings (On);

package Periodic_Tasks is

  task type Periodic_First_CPU
   (Pri    : System.Priority;
    Budget : Natural;
    Period : Positive) with CPU => 1
  is
    pragma Priority (Pri);
  end Periodic_First_CPU;

   procedure Init;
   pragma No_Return (Init);

private

  protected Initialization_Done is
    procedure Inform_Monitor (Budget : System.BB.Time.Time_Span);
  end Initialization_Done;

end Periodic_Tasks;