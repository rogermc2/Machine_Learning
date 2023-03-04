
with ML_Arrays_And_Matrices; use ML_Arrays_And_Matrices;

package Support_9A is

   function Error (Predictions : Real_Float_Vector;
                   Labels      : Integer_Matrix) return Float;
   pragma Inline (Error);

end Support_9A;
