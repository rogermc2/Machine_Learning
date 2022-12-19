
with ML_Arrays_And_Matrices; use  ML_Arrays_And_Matrices;

package ML is

   function Fit (Weights : Real_Float_Vector; Data: Integer_Matrix;
                 Labels  : Integer_Array) return Real_Float_Vector;
   function Loss (Data   : Integer_Matrix; Weights : Real_Float_Vector;
                  Labels : Integer_Array) return Float;

end ML;
