with Ada.Containers.Indefinite_Vectors;
with Ada.Numerics.Generic_Real_Arrays;

with ML_Arrays_And_Matrices; use ML_Arrays_And_Matrices;

package Structure is

   --  Sigmoid_Activation for binary output
   --  Soft_Max_Activation other output types
   type Activation_Kind is (Identity_Activation, ReLu_Activation,
                            Sigmoid_Activation, Soft_Max_Activation);
   type Loss_Kind is (Mean_Square_Error_Loss);

   type Node (Dim : Positive) is record
      Data       : Real_Float_Vector (1 .. Dim);
      Activation : Activation_Kind := Identity_Activation;
      Bias       : Float := 0.0;
      Out_Value  : Float := 0.0;
   end record;

   package Nodes_Package is new
     Ada.Containers.Indefinite_Vectors (Positive, Node);
   subtype Node_List is Nodes_Package.Vector;

   type Layer (Dim : Positive) is record
--        Data       : Real_Float_Vector (1 .. Dim);
      Nodes      : Node_List;
      Activation : Activation_Kind := Identity_Activation;
   end record;

   package Layer_Packge is new
     Ada.Containers.Indefinite_Vectors (Positive, Layer);
   subtype Layer_List is Layer_Packge.Vector;

   --     package Neural_Network is new Ada.Containers.Vectors (Positive, Node_List);

   type Sequential_Model (Input_Size : Positive) is private;

   procedure Add_Layer (aModel     : in out Sequential_Model; Layer_Size : Positive;
                       Activation : Activation_Kind := Identity_Activation);
   procedure Add_Node (aLayer     : in out Layer; Node_Size : Positive;
                       Activation : Activation_Kind := Identity_Activation);
   procedure Compile (aModel : in out Sequential_Model; Loss_Method : Loss_Kind);
   function Get_Output_Layer (aModel : Sequential_Model) return Layer;
   procedure Make_Connections (aModel : in out Sequential_Model);

private
   type Sequential_Model (Input_Size : Positive) is record
      Input_Data   : Real_Float_Vector (1 .. Input_Size);
      Layers       : Layer_List;
      Connect_List : Float_Matrix_List;
   end record;

end Structure;
