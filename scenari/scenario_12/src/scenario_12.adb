with Periodic_Tasks;
with System;

pragma Warnings (Off);
with System.BB.Threads.Queues;
pragma Warnings (On);

procedure scenario_12 is
   pragma Priority (System.Priority'Last);
   pragma CPU (1);
begin
   System.BB.Threads.Queues.Running_Thread.Is_Monitored := False;
   Periodic_Tasks.Init;
end scenario_12;