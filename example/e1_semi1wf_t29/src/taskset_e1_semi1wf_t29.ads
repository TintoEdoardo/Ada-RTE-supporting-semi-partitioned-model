with Periodic_Tasks;
use Periodic_Tasks;

package taskset_e1_semi1wf_t29 is

  T_6 : High_Crit (Id => 6, Pri => 0, Hosting_Migrating_Tasks_Priority => 0, Low_Critical_Budget => 24921, High_Critical_Budget => 49842, Workload => 1, Period => 150000, Reduced_Deadline => 91611, CPU_Id => 1);
  T_5 : High_Crit (Id => 5, Pri => 1, Hosting_Migrating_Tasks_Priority => 1, Low_Critical_Budget => 3174, High_Critical_Budget => 6348, Workload => 1, Period => 70000, Reduced_Deadline => 25160, CPU_Id => 1);
  T_2 : High_Crit (Id => 2, Pri => 2, Hosting_Migrating_Tasks_Priority => 2, Low_Critical_Budget => 1474, High_Critical_Budget => 2949, Workload => 1, Period => 60000, Reduced_Deadline => 16635, CPU_Id => 1);
  T_8 : Low_Crit (Id => 8, Pri => 3, Hosting_Migrating_Tasks_Priority => 3, On_Target_Core_Priority => -1, Low_Critical_Budget => 37638, Is_Migrable => False, Workload => 1, Period => 120000, Reduced_Deadline => 114274, CPU_Id => 1);
  T_9 : Low_Crit (Id => 9, Pri => 4, Hosting_Migrating_Tasks_Priority => 5, On_Target_Core_Priority => -1, Low_Critical_Budget => 3912, Is_Migrable => False, Workload => 1, Period => 60000, Reduced_Deadline => 58187, CPU_Id => 1);
  T_4 : Low_Crit (Id => 4, Pri => 5, Hosting_Migrating_Tasks_Priority => 6, On_Target_Core_Priority => -1, Low_Critical_Budget => 1812, Is_Migrable => False, Workload => 1, Period => 50000, Reduced_Deadline => 50000, CPU_Id => 1);
  T_7 : High_Crit (Id => 7, Pri => 0, Hosting_Migrating_Tasks_Priority => -1, Low_Critical_Budget => 15849, High_Critical_Budget => 31698, Workload => 1, Period => 120000, Reduced_Deadline => 56491, CPU_Id => 2);
  T_0 : High_Crit (Id => 0, Pri => 1, Hosting_Migrating_Tasks_Priority => -1, Low_Critical_Budget => 4865, High_Critical_Budget => 9731, Workload => 1, Period => 90000, Reduced_Deadline => 31357, CPU_Id => 2);
  T_1 : High_Crit (Id => 1, Pri => 2, Hosting_Migrating_Tasks_Priority => -1, Low_Critical_Budget => 3233, High_Critical_Budget => 6466, Workload => 1, Period => 80000, Reduced_Deadline => 24590, CPU_Id => 2);
  T_10 : Low_Crit (Id => 10, Pri => 4, Hosting_Migrating_Tasks_Priority => -1, On_Target_Core_Priority => -1, Low_Critical_Budget => 51534, Is_Migrable => False, Workload => 1, Period => 130000, Reduced_Deadline => 129113, CPU_Id => 2);
  T_3 : Low_Crit (Id => 3, Pri => 3, Hosting_Migrating_Tasks_Priority => -1, On_Target_Core_Priority => -1, Low_Critical_Budget => 2987, Is_Migrable => False, Workload => 1, Period => 140000, Reduced_Deadline => 87578, CPU_Id => 2);
  T_11 : Low_Crit (Id => 11, Pri => 5, Hosting_Migrating_Tasks_Priority => -1, On_Target_Core_Priority => 4, Low_Critical_Budget => 886, Is_Migrable => True, Workload => 1, Period => 90000, Reduced_Deadline => 90000, CPU_Id => 2);

end taskset_e1_semi1wf_t29;
