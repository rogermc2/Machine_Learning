
with ML_Arrays_And_Matrices; use ML_Arrays_And_Matrices;
with ML_U8_Types; use ML_U8_Types;
with Python;
with Python_API;

package Python_U8 is

   procedure Call (M : Python.Module; Function_Name : String;
                   A : Unsigned_8_Array_3D);
   procedure Call (M : Python.Module; Function_Name : String;
                   A : Python_API.PyObject; B : Image_64_Vector;
                   C : Integer_Array; D : Image_64_Vector;
                   E : Integer_Array);

end Python_U8;
