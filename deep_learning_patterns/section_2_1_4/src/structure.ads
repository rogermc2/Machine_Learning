with Ada.Containers.Indefinite_Vectors;
with Ada.Numerics.Generic_Real_Arrays;

with ML_Arrays_And_Matrices; use ML_Arrays_And_Matrices;

package Structure is

--     package Real_Float_Arrays is new Ada.Numerics.Generic_Real_Arrays (Float);
--     subtype Real_Float_Matrix is Real_Float_Arrays.Real_Matrix;
--     subtype Real_Float_Vector is Real_Float_Arrays.Real_Vector;

--     use Real_Float_Arrays;
--     package Float_Vector_Package is new
--       Ada.Containers.Indefinite_Vectors (Positive, Real_Float_Vector);
--     subtype Float_Vector_List is Float_Vector_Package.Vector;

--     subtype Network_Level is Real_Float_Vector;

   type Node is record
      null;
   end record;

--     package Nodes_Packge is new Ada.Containers.Vectors (Positive, Node);
--     subtype Node_List is Nodes_Packge.Vector;
--     package Neural_Network is new Ada.Containers.Vectors (Positive, Node_List);

end Structure;
