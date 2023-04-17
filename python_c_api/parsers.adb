
with Interfaces.C; use Interfaces.C;

package body Parsers is

   function Parse_Tuple (Tuple : PyObject) return ML_Types.Integer_List_2D is
      use ML_Types;
      --           Routine_Name : constant String := "Python_CLF.Call IL2D.Parse  ";
      Tuple_Size     : constant int := PyTuple_Size (Tuple);
      Tuple_Row_Size : constant int := PyTuple_Size (PyTuple_GetItem (Tuple, 1));
      Tuple_Row      : PyObject;
      Tuple_Item     : PyObject;
      Result_Row     : Integer_List;
      Value          : Integer;
      Result         : Integer_List_2D;
   begin
      for row in 0 .. Tuple_Size - 1 loop
         Tuple_Row := PyTuple_GetItem (Tuple, row);
         Result_Row.Clear;
         for col in 0 .. Tuple_Row_Size - 1 loop
            Tuple_Item := PyTuple_GetItem (Tuple_Row, col);
            Value := Integer (PyLong_AsLong (Tuple_Item));
            Result_Row.Append (Value);
         end loop;

         Result.Append (Result_Row);
      end loop;

      return Result;

   end Parse_Tuple;

end Parsers;
