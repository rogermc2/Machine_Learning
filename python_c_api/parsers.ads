
with ML_Types;
with ML_Arrays_And_Matrices; use ML_Arrays_And_Matrices;
with Python_API; use Python_API;

package Parsers is

   procedure Parse_Tuple (Tuple : PyObject_Ptr; Vec : in out Boolean_Array);
   procedure Parse_Tuple (Tuple : PyObject_Ptr; Vec : in out Integer_Array);
   function Parse_Tuple (Tuple : PyObject_Ptr) return ML_Types.Integer_List_2D;
   function Parse_Tuple (Tuple : PyObject_Ptr) return Float_Array;
   function Parse_Tuple (Tuple : PyObject_Ptr) return Integer_Array;
   function Parse_Tuple (Tuple : PyObject_Ptr) return Integer_Matrix;
   function Parse_Tuple (Tuple : PyObject_Ptr) return Real_Float_Matrix;
   function Parse_Tuple (Tuple : PyObject_Ptr) return Real_Float_Vector;

end Parsers;
