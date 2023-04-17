
with ML_Types;
with ML_Arrays_And_Matrices; use ML_Arrays_And_Matrices;
with Python_API; use Python_API;

package Parsers is

   function Parse_Tuple (Tuple : PyObject) return ML_Types.Integer_List_2D;
   procedure Parse_Tuple (Tuple : PyObject; Vec : in out Boolean_Array);
   procedure Parse_Tuple (Tuple : PyObject; Vec : in out Real_Float_Vector);

end Parsers;
