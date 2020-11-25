with Periodic_Tasks;
use Periodic_Tasks;

package taskset_e1_semi1bf_t3122 is

  T_3 : High_Crit (Pri => 3, Hosting_Migrating_Tasks_Priority => 45, Low_Critical_Budget => 100_000, High_Critical_Budget => 2_000_000, Workload => 100_000, Period => 700_000, CPU_Id => 1);

  T_0 : High_Crit (Pri => 0, Hosting_Migrating_Tasks_Priority => 0, Low_Critical_Budget => 100_000, High_Critical_Budget => 2_000_000, Workload => 100_000, Period => 700_000, CPU_Id => 2);
  
  T_8 : Low_Crit (Pri => 8, Hosting_Migrating_Tasks_Priority => 9, On_Target_Core_Priority => 0, Low_Critical_Budget => 100_000, Is_Migrable => False, Workload => 15_000, Period => 300_000, CPU_Id => 1);

  T_6 : Low_Crit (Pri => 6, Hosting_Migrating_Tasks_Priority => 0, On_Target_Core_Priority => 0, Low_Critical_Budget => 100_000, Is_Migrable => False, Workload => 1, Period => 300_000, CPU_Id => 2);

  T_1 : Low_Crit (Pri => 1, Hosting_Migrating_Tasks_Priority => 0, On_Target_Core_Priority => 4, Low_Critical_Budget => 100_000, Is_Migrable => True, Workload => 1, Period => 200_000, CPU_Id => 2);

end taskset_e1_semi1bf_t3122;
