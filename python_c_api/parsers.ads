
with ML_Types;
with Python_API; use Python_API;

package Parsers is

   function Parse_Tuple (Tuple : PyObject) return ML_Types.Integer_List_2D;

end Parsers;
