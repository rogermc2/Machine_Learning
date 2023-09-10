with Ada.Containers.Indefinite_Vectors;
with Ada.Numerics.Generic_Real_Arrays;

with ML_Arrays_And_Matrices; use ML_Arrays_And_Matrices;

package Structure_V2 is

   --  Sigmoid_Activation for binary output
   --  Soft_Max_Activation other output types
   type Activation_Kind is (Identity_Activation, ReLu_Activation,
                            Sigmoid_Activation, Soft_Max_Activation);
   type Loss_Kind is (Mean_Square_Error_Loss);

   subtype Node is Float;
--     type Node is new Real_Float_Vector;
--
--     package Nodes_Package is new
--       Ada.Containers.Indefinite_Vectors (Positive, Node);
--     subtype Node_List is Nodes_Package.Vector;

   type Connection (Dim1, Dim2 : Positive) is record
      Connection_Matrix  : Real_Float_Matrix (1 .. Dim1, 1 .. Dim2);
      Bias       : Float := 0.0;
      Activation : Activation_Kind := Identity_Activation;
   end record;

   package Connection_Package is new
     Ada.Containers.Indefinite_Vectors (Positive, Connection);
   subtype Connection_List is Connection_Package.Vector;

   type Layer (Num_Features : Positive) is record
      Nodes : Real_Float_Vector (1 .. Num_Features);
   end record;

   package Layer_Packge is new
     Ada.Containers.Indefinite_Vectors (Positive, Layer);
   subtype Layer_List is Layer_Packge.Vector;

   type Sequential_Model (Num_Features : Positive) is private;

   procedure Add_Layer (aModel     : in out Sequential_Model;
                        Num_Nodes  : Positive;
                        Input_Data : Real_Float_Vector);
   procedure Add_Layer (aModel     : in out Sequential_Model;
                        Num_Nodes  : Positive);
--     procedure Add_Node (aLayer     : in out Layer; Features : Real_Float_Vector);
   procedure Compile (aModel      : in out Sequential_Model;
                      Loss_Method : Loss_Kind);
--     function Get_Output_Value (aModel : Sequential_Model)
--                                return Real_Float_Vector;

private
   type Sequential_Model (Num_Features : Positive) is record
      Input_Data  : Real_Float_Vector (1 .. Num_Features);
      Labels      : Real_Float_Vector (1 .. 1);
      Layers      : Layer_List;
      Connections : Connection_List;
   end record;

end Structure_V2;
