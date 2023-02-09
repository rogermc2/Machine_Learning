
with ML_Arrays_And_Matrices; use ML_Arrays_And_Matrices;

package Support is

   function Fit (Data : Real_Float_Matrix) return Real_Float_Vector;
   function Load_Data (Num_Samples : Positive) return Real_Float_Matrix;

end Support;
