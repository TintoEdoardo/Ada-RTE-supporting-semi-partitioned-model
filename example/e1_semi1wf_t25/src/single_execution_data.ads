with System.Multiprocessors;
use System.Multiprocessors;

package Single_Execution_Data is
	pragma Preelaborate;

	Experiment_Hyperperiods : array (CPU) of Natural := (CPU'First => 2520000, CPU'Last => 4_880_000);

	Id_Experiment : Integer := 1;
	Approach : String := "SEMI1WF";
	Taskset_Id : Integer := 25;

	Id_Execution : String := "e1_semi1wf_t25";
	
	--  Needed to plot diagrams. These data are stored as strings in order to avoid issue related
	--  to differents types representations in differents languages (Python and Ada).
	Taskset_Size : String := "12";
	Taskset_Utilization : String := "1.8760000000000003";
	Criticality_Factor : String := "2";
	HI_Crit_Proportion : String := "0.5";

end Single_Execution_Data;