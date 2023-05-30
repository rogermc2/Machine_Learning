
with ML_U8_Types;
with Python_API; use Python_API;

package U8_Tuple_Builder is

   function To_Tuple (Data : ML_U8_Types.Unsigned_8_Array_3D)
                      return PyObject_Ptr;
   function To_Tuple (Data : ML_U8_Types.Image_64_Vector) return PyObject_Ptr;

end U8_Tuple_Builder;
