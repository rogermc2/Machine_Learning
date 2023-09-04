with Ada.Containers.Indefinite_Vectors;
with Ada.Numerics.Generic_Real_Arrays;

with ML_Arrays_And_Matrices; use ML_Arrays_And_Matrices;

package Structure is

   --     use Real_Float_Arrays;
   --     package Float_Vector_Package is new
   --       Ada.Containers.Indefinite_Vectors (Positive, Real_Float_Vector);
   --     subtype Float_Vector_List is Float_Vector_Package.Vector;

   --     subtype Network_Level is Real_Float_Vector;

   --  Sigmoid_Activation for binary output
   --  Soft_Max_Activation other output types
   type Activation_Kind is (Identity_Activation, ReLu_Activation,
                            Sigmoid_Activation, Soft_Max_Activation);
   type Loss_Kind is (Mean_Square_Error_Loss);

   type Node (Dim : Positive) is record
      Level      : Real_Float_Vector (1 .. Dim);
      Activation : Activation_Kind := Identity_Activation;
      Bias       : Float := 0.0;
   end record;

      package Nodes_Packge is new Ada.Containers.Indefinite_Vectors (Positive, Node);
      subtype Node_List is Nodes_Packge.Vector;
   --     package Neural_Network is new Ada.Containers.Vectors (Positive, Node_List);

   type Model (Input_Size : Positive) is private;

   procedure Add_Node (aModel : in out Model; Node_Size : Positive;
                       Activation : Activation_Kind := Identity_Activation);
   procedure Make_Connections (aModel : in out Model);
--     function Connect (Data : Real_Float_Vector; Level_1 : Node)
--                       return Real_Float_Matrix;
--     function Connect (Level_A, Level_B : Node) return Real_Float_Matrix;

private
   type Model (Input_Size : Positive) is record
      Input_Data   : Real_Float_Vector (1 .. Input_Size);
      Nodes        : Node_List;
      Connect_List : Float_Matrix_List;
   end record;

end Structure;
