
with ML_Arrays_And_Matrices; use  ML_Arrays_And_Matrices;

package ML is

   procedure Display_Forest (Image_Data : Unsigned_8_Array_3D);
   procedure Fit (Weights : in out Real_Float_Vector; All_Data: Integer_Matrix;
                 Labels  : Integer_Array; Verbose : Boolean := False);
   function Loss (Weights : Real_Float_Vector; All_Data : Integer_Matrix;
                  Labels  : Integer_Array) return Float;

end ML;
