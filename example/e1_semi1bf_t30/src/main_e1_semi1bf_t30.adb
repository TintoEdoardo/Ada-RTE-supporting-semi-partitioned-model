with System;
with Periodic_Tasks;

with taskset_e1_semi1bf_t30;
pragma Unreferenced (taskset_e1_semi1bf_t30);

procedure main_e1_semi1bf_t30 is
    pragma Priority (System.Priority'Last);
    pragma CPU (1);
begin
    Periodic_Tasks.Init;
end main_e1_semi1bf_t30;