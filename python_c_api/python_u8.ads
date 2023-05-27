
with ML_U8_Types;
with Python;

package Python_U8 is

   procedure Call (M : Python.Module; Function_Name : String;
                   A : ML_U8_Types.Unsigned_8_Array_3D);

end Python_U8;
