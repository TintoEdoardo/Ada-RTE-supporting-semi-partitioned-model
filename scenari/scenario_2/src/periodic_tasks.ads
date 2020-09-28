with System;

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

end Periodic_Tasks;