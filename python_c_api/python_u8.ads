
with ML_Arrays_And_Matrices;
with ML_U8_Types;
with Python;
with Python_API;

package Python_U8 is

   procedure Call (M : Python.Module; Function_Name : String;
                   A : ML_U8_Types.Unsigned_8_Array_3D);
   function Call (M : Python.Module; Function_Name : String; A : Integer;
                  B : ML_Arrays_And_Matrices.Real_Float_Matrix)
                  return Python_API.PyObject;

end Python_U8;
