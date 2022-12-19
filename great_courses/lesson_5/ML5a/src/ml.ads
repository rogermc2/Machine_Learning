
with ML_Arrays_And_Matrices; use  ML_Arrays_And_Matrices;

package ML is

   procedure Fit (Weights : in out Real_Float_Vector; Data: Integer_Matrix;
                 Labels  : Integer_Array);
   function Loss (Weights : Real_Float_Vector; Data : Integer_Matrix;
                  Labels  : Integer_Array) return Float;

end ML;
