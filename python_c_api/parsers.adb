
with Interfaces.C; use Interfaces.C;

with Ada.Assertions; use Ada.Assertions;
--  with Ada.Text_IO; use Ada.Text_IO;

package body Parsers is

   --  -------------------------------------------------------------------------

   procedure Parse_Tuple (Tuple : PyObject; Vec : in out Boolean_Array) is
      Routine_Name : constant String := "Parsers.Parse_Tuple Boolean_Array ";
      T_Row        : PyObject;
   begin
      Assert (Vec'Length = Integer (PyTuple_Size (Tuple)), Routine_Name &
                " Tuple Size " &
                int'Image (PyTuple_Size (Tuple))
              & " /= Vec Length" & Integer'Image (Vec'Length));
      for row in 1 .. PyTuple_Size (Tuple) loop
         T_Row := PyTuple_GetItem (Tuple, row - 1);
         Vec (Integer (row)) := PyObject_IsTrue (T_Row) /= 0;
      end loop;
   end Parse_Tuple;

   --  -------------------------------------------------------------------------

   procedure Parse_Tuple (Tuple : PyObject; Vec : in out Integer_Array) is
      Routine_Name : constant String := "Parsers.Parse_Tuple Integer_Array ";
      Value        : PyObject;
   begin
      Assert (Vec'Length = Integer (PyTuple_Size (Tuple)), Routine_Name &
                " Tuple Size " &
                int'Image (PyTuple_Size (Tuple))
              & " /= Vec Length" & Integer'Image (Vec'Length));
      for row in 1 .. PyTuple_Size (Tuple) loop
         Value := PyTuple_GetItem (Tuple, row - 1);
         Vec (Integer (row)) := Integer (PyLong_AsLong (Value));
      end loop;
   end Parse_Tuple;

   --  -------------------------------------------------------------------------

   function Parse_Tuple (Tuple : PyObject) return ML_Types.Integer_List_2D is
      use ML_Types;
      --           Routine_Name : constant String := "Parsers.Parse_Tuple IL2D  ";
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

   --  -------------------------------------------------------------------------

   function Parse_Tuple (Tuple : PyObject) return Integer_Matrix  is
      --        Routine_Name : constant String := "Parsers.Parse_Tuple Integer_Matrix  ";
      Tuple_Size     : constant int := PyTuple_Size (Tuple);
      Tuple_Row_Size : constant int :=
                         PyTuple_Size (PyTuple_GetItem (Tuple, 0));
      Tuple_Row      : PyObject;
      Tuple_Item     : PyObject;
      Result         : Integer_Matrix (1 .. Integer (Tuple_Size),
                                       1 .. Integer (Tuple_Row_Size));
   begin
      for row in 0 .. Tuple_Size - 1 loop
         Tuple_Row := PyTuple_GetItem (Tuple, row);
         for col in 0 .. Tuple_Row_Size - 1 loop
            Tuple_Item := PyTuple_GetItem (Tuple_Row, col);
            Result (Integer (row) + 1, Integer (col) + 1) :=
              Integer (PyLong_AsLong (Tuple_Item));
         end loop;
      end loop;

      return Result;

   end Parse_Tuple;

   --  -------------------------------------------------------------------------

   function Parse_Tuple (Tuple : PyObject) return Float_Array is
      --        Routine_Name : constant String := "Parsers.Parse_Tuple Float_Array ";
      Result       : Float_Array (1 .. Integer (PyTuple_Size (Tuple)));
   begin
      for index in Result'Range loop
         Result (index) := Float (PyFloat_AsDouble (PyTuple_GetItem
                                    (Tuple, int (index - 1))));
      end loop;

      return Result;

   end Parse_Tuple;

   --  -------------------------------------------------------------------------

   function Parse_Tuple (Tuple : PyObject) return Integer_Array is
      --        Routine_Name : constant String := "Parsers.Parse_Tuple Integer_Array ";
      Result       : Integer_Array (1 .. Integer (PyTuple_Size (Tuple)));
   begin
      for index in Result'Range loop
         Result (index) := Integer (PyInt_AsLong (PyTuple_GetItem
                                    (Tuple, int (index - 1))));
      end loop;

      return Result;

   end Parse_Tuple;

   --  -------------------------------------------------------------------------

   function Parse_Tuple (Tuple : PyObject) return Real_Float_Matrix  is
      --        Routine_Name : constant String := "Parsers.Parse_Tuple Integer_Matrix  ";
      Tuple_Size     : constant int := PyTuple_Size (Tuple);
      Tuple_Row_Size : constant int :=
                         PyTuple_Size (PyTuple_GetItem (Tuple, 0));
      Tuple_Row      : PyObject;
      Tuple_Item     : PyObject;
      Result         : Real_Float_Matrix (1 .. Integer (Tuple_Size),
                                          1 .. Integer (Tuple_Row_Size));
   begin
      for row in 0 .. Tuple_Size - 1 loop
         Tuple_Row := PyTuple_GetItem (Tuple, row);
         for col in 0 .. Tuple_Row_Size - 1 loop
            Tuple_Item := PyTuple_GetItem (Tuple_Row, col);
            Result (Integer (row) + 1, Integer (col) + 1) :=
              Float (PyFloat_AsDouble (Tuple_Item));
         end loop;
      end loop;

      return Result;

   end Parse_Tuple;

   --  -------------------------------------------------------------------------

   function Parse_Tuple (Tuple : PyObject) return Real_Float_Vector is
      --        Routine_Name : constant String := "Parsers.Parse_Tuple RFV ";
      Result       : Real_Float_Vector (1 .. Integer (PyTuple_Size (Tuple)));
   begin
      for index in Result'Range loop
         Result (index) := Float (PyFloat_AsDouble (PyTuple_GetItem
                                  (Tuple, int (index - 1))));
      end loop;

      return Result;

   end Parse_Tuple;

   --  -------------------------------------------------------------------------

end Parsers;
