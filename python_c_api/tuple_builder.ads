
--  Based on inspirel_ada-python_demo

with ML_Arrays_And_Matrices;
with ML_Types;
with NL_Types;
with Python_API; use Python_API;

package Tuple_Builder is

   function To_Tuple (Data : ML_Arrays_And_Matrices.Integer_Array) 
                      return PyObject;
   function To_Tuple (Data : ML_Arrays_And_Matrices.Integer_Array_List)
                      return PyObject;
   function To_Tuple (Data : ML_Types.Integer_List) return PyObject;
   function To_Tuple (Data : ML_Arrays_And_Matrices.Integer_Matrix) 
                      return PyObject; 
   function To_Tuple (Data : ML_Arrays_And_Matrices.Unsigned_8_Array_3D) 
                      return PyObject;
   function To_Tuple (Data : NL_Types.Boolean_List) return PyObject;
   function To_Tuple (Data : NL_Types.Boolean_List_2D) return PyObject;
   function To_Tuple (Data : ML_Types.Bounded_String_List) return PyObject;
   function To_Tuple (Data : NL_Types.Float_List) return PyObject;
   function To_Tuple (Data : ML_Arrays_And_Matrices.Real_Float_List) 
                      return PyObject;
   function To_Tuple (Data : ML_Arrays_And_Matrices.Real_Float_Matrix) 
                      return PyObject;
   function To_Tuple (Data : ML_Arrays_And_Matrices.Real_Float_Vector) 
                      return PyObject;
   function To_Tuple (Data : ML_Types.Indef_String_List) return PyObject;
   function To_Tuple (Data : ML_Types.Unbounded_List) return PyObject;
   
end Tuple_Builder;
