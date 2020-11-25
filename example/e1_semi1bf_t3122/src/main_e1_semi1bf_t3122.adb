with System;
with Periodic_Tasks;

with taskset_e1_semi1bf_t3122;
pragma Unreferenced (taskset_e1_semi1bf_t3122);

procedure main_e1_semi1bf_t3122 is
    pragma Priority (System.Priority'Last);
    pragma CPU (1);
begin
    Periodic_Tasks.Init (Id_Experiment => 1, Approach => "SEMI-1 BF", Taskset_Id => 3122);
end main_e1_semi1bf_t3122;