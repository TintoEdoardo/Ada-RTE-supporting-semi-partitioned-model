with Periodic_Tasks;
use Periodic_Tasks;

package taskset_e1_semi1wf_t25 is

  T_4 : High_Crit (Id => 4, Pri => 1, Hosting_Migrating_Tasks_Priority => 1, Low_Critical_Budget => 1_000_000, High_Critical_Budget => 1_000_000, Workload => 13_258, Period => 800_000, Reduced_Deadline => 800_000, CPU_Id => 1);

end taskset_e1_semi1wf_t25;
