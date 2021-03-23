with Periodic_Tasks;
use Periodic_Tasks;

package taskset_e1_semi1wf_t25 is

  --  T_4 : High_Crit (Id => 4, Pri => 1, Hosting_Migrating_Tasks_Priority => 1, Low_Critical_Budget => 13474, High_Critical_Budget => 26948, Workload => 1, Period => 90000, Reduced_Deadline => 90000, CPU_Id => 1);
  --  T_10 : High_Crit (Id => 10, Pri => 0, Hosting_Migrating_Tasks_Priority => 0, Low_Critical_Budget => 5481, High_Critical_Budget => 10962, Workload => 1, Period => 140000, Reduced_Deadline => 140000, CPU_Id => 1);
  --  T_0 : Low_Crit (Id => 0, Pri => 2, Hosting_Migrating_Tasks_Priority => 2, On_Target_Core_Priority => -1, Low_Critical_Budget => 31646, Is_Migrable => False, Workload => 1, Period => 100000, Reduced_Deadline => 100000, CPU_Id => 1);
  --  T_1 : Low_Crit (Id => 1, Pri => 3, Hosting_Migrating_Tasks_Priority => 3, On_Target_Core_Priority => -1, Low_Critical_Budget => 14497, Is_Migrable => False, Workload => 1, Period => 100000, Reduced_Deadline => 100000, CPU_Id => 1);
  --  T_3 : Low_Crit (Id => 3, Pri => 4, Hosting_Migrating_Tasks_Priority => 5, On_Target_Core_Priority => -1, Low_Critical_Budget => 2095, Is_Migrable => False, Workload => 1, Period => 60000, Reduced_Deadline => 60000, CPU_Id => 1);
  --  T_5 : High_Crit (Id => 5, Pri => 3, Hosting_Migrating_Tasks_Priority => -1, Low_Critical_Budget => 5643, High_Critical_Budget => 11287, Workload => 1, Period => 80000, Reduced_Deadline => 34288, CPU_Id => 2);
  --  T_8 : High_Crit (Id => 8, Pri => 0, Hosting_Migrating_Tasks_Priority => -1, Low_Critical_Budget => 8266, High_Critical_Budget => 16533, Workload => 1, Period => 170000, Reduced_Deadline => 101737, CPU_Id => 2);
  --  T_11 : High_Crit (Id => 11, Pri => 1, Hosting_Migrating_Tasks_Priority => -1, Low_Critical_Budget => 4388, High_Critical_Budget => 8777, Workload => 1, Period => 110000, Reduced_Deadline => 55315, CPU_Id => 2);
  --  T_9 : High_Crit (Id => 9, Pri => 2, Hosting_Migrating_Tasks_Priority => -1, Low_Critical_Budget => 3329, High_Critical_Budget => 6659, Workload => 1, Period => 110000, Reduced_Deadline => 58644, CPU_Id => 2);
  --  T_2 : Low_Crit (Id => 2, Pri => 4, Hosting_Migrating_Tasks_Priority => -1, On_Target_Core_Priority => -1, Low_Critical_Budget => 28429, Is_Migrable => False, Workload => 1, Period => 120000, Reduced_Deadline => 102717, CPU_Id => 2);
  --  T_6 : Low_Crit (Id => 6, Pri => 6, Hosting_Migrating_Tasks_Priority => -1, On_Target_Core_Priority => -1, Low_Critical_Budget => 9188, Is_Migrable => False, Workload => 1, Period => 60000, Reduced_Deadline => 60000, CPU_Id => 2);
  T_7 : Low_Crit (Id => 7, Pri => 5, Hosting_Migrating_Tasks_Priority => -1, On_Target_Core_Priority => 4, Low_Critical_Budget => 8093, Is_Migrable => True, Workload => 1, Period => 80000, Reduced_Deadline => 70811, CPU_Id => 2);

end taskset_e1_semi1wf_t25;
