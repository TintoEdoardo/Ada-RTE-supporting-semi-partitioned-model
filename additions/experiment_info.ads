with System.Multiprocessors;
use System.Multiprocessors;

with Ada.Strings.Unbounded;

--  The application should use this package in order to get the runtime system
--  aware about experiment parameters. This parameters are get from
--  "Single_Execution_Data" application's package.

package Experiment_Info is
   pragma Preelaborate;

   package SU renames Ada.Strings.Unbounded;

   type HP_array is array (CPU) of Natural;

   type Exp_Params is record
      Experiment_Hyperperiods : HP_array;

      Id_Experiment : Integer;
      Approach : SU.Unbounded_String;
      Taskset_Id : Integer;

      Id_Execution : SU.Unbounded_String;
   end record;

   procedure Set_Parameters (Params : Exp_Params);

   function Get_Parameters return Exp_Params;

end Experiment_Info;
