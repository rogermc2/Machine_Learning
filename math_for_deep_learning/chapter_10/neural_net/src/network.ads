
with Ada.Containers.Indefinite_Vectors;

with ML_Arrays_And_Matrices; use ML_Arrays_And_Matrices;
--  with ML_Types;

with Neural_Processes; use Neural_Processes;

package Network is

   package Network_Layers is new Ada.Containers.Indefinite_Vectors
     (Positive, Neural_Processes.Layer_Data);
   subtype Network_List is Network_Layers.Vector;

   type Network_Data is record
      Verbose : Boolean := False;
      Layers  : Network_List;
   end record;

   procedure Add (Network : in out Network_Data; Layer : Layer_Data);
   function Predict (Network : in out Network_Data; Input_Data : Real_Float_Matrix)
                     return Real_Matrix_List;

end Network;
