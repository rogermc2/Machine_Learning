
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

   procedure Add_Activation_Layer (Network : in out Network_List);
   procedure Add_Activation_Layer (Network    : in out Network_List;
                                   Input_Size : Layer_Range);
   procedure Add_Fully_Connected_Layer
     (Network : in out Network_List; Input_Size, Output_Size : Layer_Range);
   procedure Fit
     (Network       : in out Network_Data; X_Train : Real_Float_Matrix;
      Y_Train       : Real_Float_Matrix; Minibatches : Positive;
      Learning_Rate : Float; Batch_Size : Positive := 64);
   function Predict (Network    : in out Network_Data;
                     Input_Data : Real_Float_Matrix) return Real_Float_List_2D;
   procedure Print_Layer_Data (Name : String; Layer : Layer_Data);
   procedure Print_Network_Data (Name : String; Network : Network_Data);

end Network;
