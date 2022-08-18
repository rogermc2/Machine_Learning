
with NL_Arrays_And_Matrices; use NL_Arrays_And_Matrices;

with Connectionist_Types; use Connectionist_Types;

package Training is

   procedure Train_Neuron
     (Neuron                                    : in out Processing_Element;
      Num_Data, Num_Layers                      : in out Natural;
      Active_Layer, Record_Weights, Write_Error : Boolean);
   function  Initialize_Training
     (Num_Data, Num_Layers : out Natural; Layer_Entries : out Layer_List;
      Active_Layer : out Boolean) return Output_Type;

end Training;
