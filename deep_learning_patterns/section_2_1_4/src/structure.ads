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
      Features  : Real_Float_Vector (1 .. Dim);
      Weights   : Real_Float_Vector (1 .. Dim);
      Bias      : Float := 0.0;
   end record;

   package Nodes_Package is new
     Ada.Containers.Indefinite_Vectors (Positive, Node);
   subtype Node_List is Nodes_Package.Vector;

   type Layer (Num_Nodes, Dim : Positive) is record
      Activation  : Activation_Kind := Identity_Activation;
      Nodes       : Node_List;
      Output_Data : Real_Float_Vector (1 .. Num_Nodes);
      Loss        : Real_Float_Vector (1 .. Num_Nodes);
   end record;

   package Layer_Packge is new
     Ada.Containers.Indefinite_Vectors (Positive, Layer);
   subtype Layer_List is Layer_Packge.Vector;

   --     package Neural_Network is new Ada.Containers.Vectors (Positive, Node_List);

   type Sequential_Model (Num_Features : Positive) is private;

   procedure Add_Layer (aModel     : in out Sequential_Model;
                        Num_Nodes  : Positive;
                        Input_Data : Real_Float_Vector;
                        Activation : Activation_Kind := Identity_Activation);
   procedure Add_Layer (aModel     : in out Sequential_Model;
                        Num_Nodes  : Positive;
                        Activation : Activation_Kind := Identity_Activation);
   procedure Add_Node (aLayer     : in out Layer; Features : Real_Float_Vector);
   procedure Compile (aModel      : in out Sequential_Model;
                      Loss_Method : Loss_Kind);
   function Get_Output_Value (aModel : Sequential_Model)
                              return Real_Float_Vector;

private
   type Sequential_Model (Num_Features : Positive) is record
      Input_Data   : Real_Float_Vector (1 .. Num_Features);
      Labels       : Real_Float_Vector (1 .. 1);
      Layers       : Layer_List;
   end record;

end Structure;
