
with ML_Arrays_And_Matrices; use ML_Arrays_And_Matrices;

package Support_9AUX is

   type Data_Record (Num_Items, Num_Features : Positive) is record
      Features : Real_Float_Matrix (1 .. Num_Items, 1 .. Num_Features);
      Labels   : Integer_Matrix (1 .. Num_Items, 1 .. 1);
   end record;

   function Error (Predictions : Real_Float_Vector;
                   Labels      : Integer_Matrix) return Float;

end Support_9AUX;
