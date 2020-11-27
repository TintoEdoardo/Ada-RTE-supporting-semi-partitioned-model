with System.Multiprocessors;
use System.Multiprocessors;

package Single_Execution_Data is
	pragma Preelaborate;

	Experiment_Hyperperiods : array (CPU) of Natural := (CPU'First => 2_200_000, CPU'Last => 10_000_000);

	Id_Experiment : Integer := 1;
	Approach : String := "SEMI1BF";
	Taskset_Id : Integer := 30;

	Id_Execution : String := "e1_semi1bf_t30";

end Single_Execution_Data;