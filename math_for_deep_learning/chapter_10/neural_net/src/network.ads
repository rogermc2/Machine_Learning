
with Ada.Containers.Indefinite_Vectors;

with ML_Arrays_And_Matrices; use ML_Arrays_And_Matrices;
--  with ML_Types;

with Neural_Processes; use Neural_Processes;

package Network is

   --     type Node_Data (Num_Inputs, Num_Outputs : Positive) is record
   --        Data : Real_Float_Matrix (1 .. Num_Inputs, 1 .. Num_Outputs);
   --     end record;
   --
   --     type Layer_Data  (Num_Samples, Num_Inputs, Num_Outputs : Positive) is record
   --        Node_Data : array (1 .. Num_Samples) of Node_Data (Num_Inputs, Num_Outputs);
   --     end record;

   package Network_Layers is new Ada.Containers.Indefinite_Vectors
     (Positive, Neural_Processes.Layer_Data);
   subtype Network_List is Network_Layers.Vector;

   type Network_Data is record
      Verbose : Boolean := False;
      Layers  : Network_List;
   end record;

   procedure Add_Activation_Layer (Network : in out Network_List);
   procedure Add_Fully_Connected_Layer
     (Network : in out Network_List; Input_Size, Output_Size : Positive);
   procedure Fit
     (Network       : in out Network_Data; X_Train : Real_Float_Matrix;
      Y_Train       : Real_Float_Matrix; Minibatches : Positive;
      Learning_Rate : Float; Batch_Size : Positive := 64);
   function Predict (Network    : in out Network_Data;
                     Input_Data : Real_Float_Matrix)
                     return Real_Float_List_2D;

end Network;
