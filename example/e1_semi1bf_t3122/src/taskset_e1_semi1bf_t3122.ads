with Periodic_Tasks;
use Periodic_Tasks;

package taskset_e1_semi1bf_t3122 is

  T_4 : High_Crit (Pri => 0, Hosting_Migrating_Tasks_Priority => -1, Low_Critical_Budget => 639369, High_Critical_Budget => 1278738, Workload => 700_000, Period => 5300000, CPU_Id => 1);
  T_6 : High_Crit (Pri => 2, Hosting_Migrating_Tasks_Priority => -1, Low_Critical_Budget => 55746, High_Critical_Budget => 111493, Workload => 1, Period => 1100000, CPU_Id => 1);
  T_3 : High_Crit (Pri => 1, Hosting_Migrating_Tasks_Priority => -1, Low_Critical_Budget => 57954, High_Critical_Budget => 115909, Workload => 1, Period => 2300000, CPU_Id => 1);
  T_10 : Low_Crit (Pri => 4, Hosting_Migrating_Tasks_Priority => -1, On_Target_Core_Priority => -1, Low_Critical_Budget => 166839, Is_Migrable => False, Workload => 1, Period => 600000, CPU_Id => 1);
  T_11 : Low_Crit (Pri => 5, Hosting_Migrating_Tasks_Priority => -1, On_Target_Core_Priority => -1, Low_Critical_Budget => 47658, Is_Migrable => False, Workload => 1, Period => 300000, CPU_Id => 1);
  T_0 : Low_Crit (Pri => 3, Hosting_Migrating_Tasks_Priority => -1, On_Target_Core_Priority => 4 + 13, Low_Critical_Budget => 276877, Is_Migrable => True, Workload => 1, Period => 1900000, CPU_Id => 1);
  T_5 : High_Crit (Pri => 0 + 13, Hosting_Migrating_Tasks_Priority => 0 + 13, Low_Critical_Budget => 743050, High_Critical_Budget => 1486100, Workload => 1, Period => 6300000, CPU_Id => 2);
  T_1 : High_Crit (Pri => 1 + 13, Hosting_Migrating_Tasks_Priority => 1 + 13, Low_Critical_Budget => 158168, High_Critical_Budget => 316336, Workload => 1, Period => 2700000, CPU_Id => 2);
  T_2 : High_Crit (Pri => 2 + 13, Hosting_Migrating_Tasks_Priority => 3 + 13, Low_Critical_Budget => 13047, High_Critical_Budget => 26094, Workload => 1, Period => 800000, CPU_Id => 2);
  T_7 : Low_Crit (Pri => 4 + 13, Hosting_Migrating_Tasks_Priority => 5 + 13, On_Target_Core_Priority => -1, Low_Critical_Budget => 268716, Is_Migrable => False, Workload => 1, Period => 600000, CPU_Id => 2);
  T_8 : Low_Crit (Pri => 5 + 13, Hosting_Migrating_Tasks_Priority => 6 + 13, On_Target_Core_Priority => -1, Low_Critical_Budget => 18821, Is_Migrable => False, Workload => 1, Period => 600000, CPU_Id => 2);
  T_9 : Low_Crit (Pri => 3 + 13, Hosting_Migrating_Tasks_Priority => 2 + 13, On_Target_Core_Priority => -1, Low_Critical_Budget => 71979, Is_Migrable => False, Workload => 1, Period => 6300000, CPU_Id => 2);

end taskset_e1_semi1bf_t3122;
