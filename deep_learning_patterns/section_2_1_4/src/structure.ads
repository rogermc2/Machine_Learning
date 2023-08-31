with Ada.Containers.Indefinite_Vectors;
with Ada.Numerics.Generic_Real_Arrays;

with ML_Arrays_And_Matrices; use ML_Arrays_And_Matrices;

package Structure is

   --     use Real_Float_Arrays;
   --     package Float_Vector_Package is new
   --       Ada.Containers.Indefinite_Vectors (Positive, Real_Float_Vector);
   --     subtype Float_Vector_List is Float_Vector_Package.Vector;

   --     subtype Network_Level is Real_Float_Vector;

   type Activation_Kind is (Identity_Activation, ReLu_Activation,
                           Sigmoid_Activation, Soft_Max_Activation);

   type Node (Dim : Positive) is record
      Level      : Real_Float_Vector (1 .. Dim);
      Activation : Activation_Kind := Identity_Activation;
   end record;

      package Nodes_Packge is new Ada.Containers.Indefinite_Vectors (Positive, Node);
      subtype Node_List is Nodes_Packge.Vector;
   --     package Neural_Network is new Ada.Containers.Vectors (Positive, Node_List);

   function Connect (Level_A, Level_B : Node) return Real_Float_Matrix;

end Structure;
