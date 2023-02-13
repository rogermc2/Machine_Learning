
with ML_Arrays_And_Matrices; use ML_Arrays_And_Matrices;
with ML_Types;

package Support_Iris is

   function Test_Score (Predictions : Real_Float_Vector;
                        Labels      : Integer_Matrix) return Natural;

end Support_Iris;
