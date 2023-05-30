
--  Based on inspirel_ada-python_demo

with ML_Arrays_And_Matrices;
with ML_Types;
with NL_Types;
with Python_API; use Python_API;

package Tuple_Builder is

   function To_Tuple (Data : ML_Arrays_And_Matrices.Float_Array) 
                      return PyObject_Ptr;
   function To_Tuple (Data : ML_Arrays_And_Matrices.Integer_Array) 
                      return PyObject_Ptr;
   function To_Tuple (Data : ML_Arrays_And_Matrices.Float_Array_List)
                      return PyObject_Ptr;
   function To_Tuple (Data : ML_Arrays_And_Matrices.Float_Matrix_List)
                      return PyObject_Ptr;
   function To_Tuple (Data : ML_Arrays_And_Matrices.Integer_Array_List)
                      return PyObject_Ptr;
   function To_Tuple (Data : ML_Types.Integer_List) return PyObject_Ptr;
   function To_Tuple (Data : ML_Types.Integer_List_2D) return PyObject_Ptr;
   function To_Tuple (Data : ML_Arrays_And_Matrices.Integer_Matrix) 
                      return PyObject_Ptr;
   function To_Tuple (Data : ML_Arrays_And_Matrices.Boolean_Array) 
                      return PyObject_Ptr;
   function To_Tuple (Data : NL_Types.Boolean_List) return PyObject_Ptr;
   function To_Tuple (Data : NL_Types.Boolean_List_2D) return PyObject_Ptr;
   function To_Tuple (Data : ML_Arrays_And_Matrices.Real_Float_List_2D)
                      return PyObject_Ptr;
   function To_Tuple (Data : ML_Types.Bounded_String_List) return PyObject_Ptr;
   function To_Tuple (Data : NL_Types.Float_List) return PyObject_Ptr;
   function To_Tuple (Data : ML_Arrays_And_Matrices.Real_Float_List) 
                      return PyObject_Ptr;
   function To_Tuple (Data : ML_Arrays_And_Matrices.Real_Float_Matrix) 
                      return PyObject_Ptr;
   function To_Tuple (Data : ML_Arrays_And_Matrices.Real_Float_Vector) 
                      return PyObject_Ptr;
   function To_Tuple (Data : ML_Types.Indef_String_List) return PyObject_Ptr;
   function To_Tuple (Data : ML_Arrays_And_Matrices.Real_Vector_List)
                      return PyObject_Ptr;
   function To_Tuple (Data : ML_Arrays_And_Matrices.Float_Vector_List)
                      return PyObject_Ptr;
   function To_Tuple (Data : ML_Types.Unbounded_List) return PyObject_Ptr;
   function To_Tuple (Data : ML_Arrays_And_Matrices.Unbounded_String_Array)
                      return PyObject_Ptr;
   function To_Tuple (Data : ML_Arrays_And_Matrices.Unbounded_String_Matrix)
                      return PyObject_Ptr;
   
end Tuple_Builder;
