
with Ada.Containers.Indefinite_Vectors;

with ML_Arrays_And_Matrices; use ML_Arrays_And_Matrices;
--  with ML_Types;

with Dense; use Dense;

package Network is

   package Network_Layers is new Ada.Containers.Indefinite_Vectors
     (Positive, Dense.Layer_Data);
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
   procedure Train
     (Network       : in out Network_Data; X_Train : Real_Float_Matrix;
      Y_Train       : Binary_Matrix; Epochs : Positive := 1000;
      Learning_Rate : Float := 0.01; Verbose : Boolean := True);
   procedure Predict (Network : in out Network_Data; Data : Real_Float_List);
   procedure Print_Layer_Data (Name : String; Layer : Layer_Data);
   procedure Print_Network_Data (Name : String; Network : Network_Data);

end Network;
