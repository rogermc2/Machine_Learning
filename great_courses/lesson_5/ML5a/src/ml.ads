
with ML_Arrays_And_Matrices; use  ML_Arrays_And_Matrices;
with ML_U8_Types;

package ML is

   function Composite
     (Mask, Foreground, Background : ML_U8_Types.Unsigned_8_Array_3D)
      return ML_U8_Types.Unsigned_8_Array_3D;

   procedure Fit (Weights : in out Real_Float_Vector; All_Data: Integer_Matrix;
                  Labels  : Integer_Array; Verbose : Boolean := False);
   function Loss (Weights : Real_Float_Vector; All_Data : Integer_Matrix;
                  Labels  : Integer_Array) return Float;

end ML;
