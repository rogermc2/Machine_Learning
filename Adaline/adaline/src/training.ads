
with NL_Arrays_And_Matrices; use NL_Arrays_And_Matrices;

with Connectionist_Types; use Connectionist_Types;

package Training is

   procedure Train_Neuron
      (Neuron : in out Processing_Element; Num_Data, Num_Patterns : Natural;
       Active_Pattern, Record_Weights, Write_Error : Boolean);
   procedure Initialize_Training;

end Training;
