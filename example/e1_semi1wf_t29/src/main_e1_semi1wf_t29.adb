with System;
with Periodic_Tasks;

with taskset_e1_semi1wf_t29;
pragma Unreferenced (taskset_e1_semi1wf_t29);

procedure main_e1_semi1wf_t29 is
    pragma Priority (System.Priority'Last);
    pragma CPU (1);
begin
    Periodic_Tasks.Init;
end main_e1_semi1wf_t29;