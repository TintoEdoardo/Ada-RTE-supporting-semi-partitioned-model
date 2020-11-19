with Periodic_Tasks;

procedure Scenario_1 is
   pragma Priority (0);
begin
   Periodic_Tasks.Init;
end Scenario_1;