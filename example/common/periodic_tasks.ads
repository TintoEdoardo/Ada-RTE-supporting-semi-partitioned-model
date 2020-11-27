with System;

with System.Multiprocessors;
use System.Multiprocessors;

pragma Warnings (Off);
with System.BB.Time;
pragma Warnings (On);

package Periodic_Tasks is

  --  CPU_A  : constant CPU := CPU'First;
  --  CPU_B  : constant CPU := CPU'Last; 

  --  Periodic LO-CRIT task
  task type Low_Crit
      (Id                              : Natural;
      Pri                              : System.Priority;
      Hosting_Migrating_Tasks_Priority : Integer;
      On_Target_Core_Priority          : Integer;
      Low_Critical_Budget              : Natural;
      Is_Migrable                      : Boolean;
      Workload                         : Positive;
      Period                           : Positive;
      Reduced_Deadline                 : Positive;
      CPU_Id                           : CPU)
  is
      pragma Priority (Pri);
      pragma CPU (CPU_Id);
  end Low_Crit;

  --  Periodic HI-CRIT task
  task type High_Crit
      (Id                              : Natural;
      Pri                              : System.Priority;
      Hosting_Migrating_Tasks_Priority : Integer;
      Low_Critical_Budget              : Natural;
      High_Critical_Budget             : Natural;
      Workload                         : Positive;
      Period                           : Positive;
      Reduced_Deadline                 : Positive;
      CPU_Id                           : CPU) 
  is
      pragma Priority (Pri);
      pragma CPU (CPU_Id);
  end High_Crit;

  procedure Init;
  pragma No_Return (Init);

end Periodic_Tasks;