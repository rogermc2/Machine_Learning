
with ML_Arrays_And_Matrices; use ML_Arrays_And_Matrices;
--  with Python;
--  with Python_API;

package Support_7A is

   function Fit (Data : Real_Float_Matrix) return Real_Float_Vector;
   function Load_Data (Num_Samples : Positive) return Real_Float_Matrix;

end Support_7A;
