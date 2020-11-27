with Periodic_Tasks;
use Periodic_Tasks;

package taskset_e1_semi1bf_t30 is

  T_9 : High_Crit (Id => 9, Pri => 0, Hosting_Migrating_Tasks_Priority => -1, Low_Critical_Budget => 19253, High_Critical_Budget => 38507, Workload => 1, Period => 90000, Reduced_Deadline => 71758, CPU_Id => 1);
  T_5 : High_Crit (Id => 5, Pri => 1, Hosting_Migrating_Tasks_Priority => -1, Low_Critical_Budget => 12133, High_Critical_Budget => 24267, Workload => 1, Period => 80000, Reduced_Deadline => 73892, CPU_Id => 1);
  T_7 : High_Crit (Id => 7, Pri => 3, Hosting_Migrating_Tasks_Priority => -1, Low_Critical_Budget => 2026, High_Critical_Budget => 4052, Workload => 1, Period => 50000, Reduced_Deadline => 46885, CPU_Id => 1);
  T_1 : High_Crit (Id => 1, Pri => 2, Hosting_Migrating_Tasks_Priority => -1, Low_Critical_Budget => 966, High_Critical_Budget => 1933, Workload => 1, Period => 60000, Reduced_Deadline => 54859, CPU_Id => 1);
  T_11 : High_Crit (Id => 11, Pri => 4, Hosting_Migrating_Tasks_Priority => -1, Low_Critical_Budget => 462, High_Critical_Budget => 924, Workload => 1, Period => 50000, Reduced_Deadline => 47347, CPU_Id => 1);
  T_8 : Low_Crit (Id => 8, Pri => 5, Hosting_Migrating_Tasks_Priority => -1, On_Target_Core_Priority => 6, Low_Critical_Budget => 2652, Is_Migrable => True, Workload => 1, Period => 70000, Reduced_Deadline => 70000, CPU_Id => 1);
  T_2 : High_Crit (Id => 2, Pri => 0, Hosting_Migrating_Tasks_Priority => 0, Low_Critical_Budget => 33921, High_Critical_Budget => 67843, Workload => 1, Period => 250000, Reduced_Deadline => 131576, CPU_Id => 2);
  T_10 : Low_Crit (Id => 10, Pri => 5, Hosting_Migrating_Tasks_Priority => 5, On_Target_Core_Priority => -1, Low_Critical_Budget => 16361, Is_Migrable => False, Workload => 1, Period => 80000, Reduced_Deadline => 80000, CPU_Id => 2);
  T_6 : Low_Crit (Id => 6, Pri => 1, Hosting_Migrating_Tasks_Priority => 1, On_Target_Core_Priority => -1, Low_Critical_Budget => 42881, Is_Migrable => False, Workload => 1, Period => 230000, Reduced_Deadline => 163733, CPU_Id => 2);
  T_4 : Low_Crit (Id => 4, Pri => 3, Hosting_Migrating_Tasks_Priority => 3, On_Target_Core_Priority => -1, Low_Critical_Budget => 15061, Is_Migrable => False, Workload => 1, Period => 180000, Reduced_Deadline => 154362, CPU_Id => 2);
  T_0 : Low_Crit (Id => 0, Pri => 4, Hosting_Migrating_Tasks_Priority => 4, On_Target_Core_Priority => -1, Low_Critical_Budget => 9276, Is_Migrable => False, Workload => 1, Period => 120000, Reduced_Deadline => 103638, CPU_Id => 2);
  T_3 : Low_Crit (Id => 3, Pri => 2, Hosting_Migrating_Tasks_Priority => 2, On_Target_Core_Priority => -1, Low_Critical_Budget => 9205, Is_Migrable => False, Workload => 1, Period => 210000, Reduced_Deadline => 169300, CPU_Id => 2);

end taskset_e1_semi1bf_t30;
