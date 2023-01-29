
--  Based on inspirel_ada-python_demo

with Interfaces.C;

with Ada.Exceptions; use Ada.Exceptions;
with Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Unchecked_Conversion;

package body Tuple_Builder is

   --  ------------------------------------------------------------------------------

   function To_Tuple (Data : ML_Arrays_And_Matrices.Integer_Array) 
                      return PyObject is
      use Interfaces.C;
      Routine_Name : constant String := "Python.To_Tuple Integer_Matrix ";
      Value        : Integer;
      Py_Row       : int := -1;
      Result       : constant PyObject := PyTuple_New (int (Data'Length));
   begin
      for row in Data'Range loop
         Py_Row := Py_Row + 1;
         Value := Data (row);
         PyTuple_SetItem (Result, Py_Row, PyLong_FromLong (long (Value)));
      end loop;

      return Result;

   exception
      when E : others =>
         Put_Line (Routine_Name & "error" & Exception_Message (E));
         raise;

   end To_Tuple;

   --  -------------------------------------------------------------------------

   function To_Tuple (Data : ML_Arrays_And_Matrices.Integer_Array_List)
                      return PyObject is
      use Interfaces.C;
      use ML_Arrays_And_Matrices;
      
      function Py_BuildValue (Format : char_array; T1 : int) return PyObject;
      pragma Import (C, Py_BuildValue, "Py_BuildValue");
      
--        Routine_Name : constant String := "Python.To_Tuple Integer_Array_List ";
      Tuple        : PyObject;
      Py_Index     : int := -1;
   begin
      Tuple := PyTuple_New (int (Data.Length));
      for row in Data.First_Index .. Data.Last_Index loop
         Py_Index := Py_Index + 1;
         declare
            Row_Data     : constant Integer_Array := Data (row);
            PyParams     : PyObject;
            Row_Tuple    : PyObject;
            Py_Row_Index : int := -1;
         begin
            Row_Tuple := PyTuple_New (int (Row_Data'Length));
            for index in Row_Data'Range loop
               Py_Row_Index := Py_Row_Index + 1;
               PyParams := Py_BuildValue (To_C ("(i)"), int (Row_Data (index)));
               PyTuple_SetItem (Row_Tuple, Py_Row_Index, PyParams); 
               Py_DecRef (PyParams);
            end loop;
            PyTuple_SetItem (Tuple, Py_Index, To_Tuple (Row_Data));
         end;
      end loop;

      return Tuple;

   end To_Tuple;

   --  -------------------------------------------------------------------------

   function To_Tuple (Data : ML_Arrays_And_Matrices.Integer_Matrix) 
                      return PyObject is
      use Interfaces.C;
--        Routine_Name : constant String := "Python.To_Tuple Integer_Matrix ";
      Num_Cols     : constant Positive := Data'Length (2);
      Row_Size     : constant int := int (Num_Cols);
      Value        : Integer;
      Long_Value   : long;
      Item         : PyObject;
      Py_Row       : int := -1;
      Py_Col       : int := -1;
      Py_Matrix    : constant PyObject := PyTuple_New (int (Data'Length));
      Result       : constant PyObject := PyTuple_New (1);
   begin
      for row in Data'Range loop
         Item := PyTuple_New (Row_Size);
         Py_Row := Py_Row + 1;
         Py_Col := -1;
         for col in Data'Range (2) loop
            Py_Col := Py_Col + 1;
            Value := Data (row, col);
            Long_Value := long (Value);
            PyTuple_SetItem (Item, Py_Col, PyLong_FromLong (Long_Value));
         end loop;
         PyTuple_SetItem (Py_Matrix, Py_Row, Item);
      end loop;
      PyTuple_SetItem (Result, 0, Py_Matrix);

      return Result;

   end To_Tuple;

   --  -------------------------------------------------------------------------

   function To_Tuple (Data : ML_Arrays_And_Matrices.Unsigned_8_Array_3D) 
                      return PyObject is
      use ML_Arrays_And_Matrices;
--        Routine_Name : constant String := "Python.To_Tuple Integer_Array_3D ";
      Array_Length : constant Positive := Data'Length (1) * Data'Length (2);
      Array_2D     : Integer_Matrix (1 .. Array_Length, Data'Range (3));
   begin
      for row in Data'Range loop
         for col in Data'Range (2) loop
            for depth in Data'Range (3) loop
               Array_2D ((row - 1) * Data'Length (2) + col, depth) :=
                 Integer (Data (row, col, depth));
            end loop;
         end loop;
      end loop;

      return To_Tuple (Array_2D);

   end To_Tuple;

   --  -------------------------------------------------------------------------

   function To_Tuple (Data : NL_Types.Boolean_List) return PyObject is
      use Interfaces.C;
--        Routine_Name : constant String := "Python.To_Tuple Boolean_List ";
      Tuple_2D     : constant PyObject := PyTuple_New (int (Data.Length));
      Long_Value   : long;
      Py_Index     : int := -1;
   begin
      for index in Data.First_Index .. Data.Last_Index loop
         Py_Index := Py_Index + 1;
         if Data (index) then
            Long_Value := 1;
         else
            Long_Value := 0;
         end if;
         PyTuple_SetItem (Tuple_2D, Py_Index, PyBool_FromLong (Long_Value));
      end loop;

      return Tuple_2D;

   end To_Tuple;

   --  -------------------------------------------------------------------------

   function To_Tuple (Data : NL_Types.Boolean_List_2D) return PyObject is
      use Interfaces.C;
--        Routine_Name : constant String := "Python.To_Tuple Boolean_List_2D ";
      Row_Size     : int;
      Long_Value   : long;
      Tuple_2D     : PyObject;
      Tuple        : PyObject;
      Py_Row       : int := -1;
      Py_Col       : int := -1;
   begin
      Tuple_2D := PyTuple_New (int (Data.Length));
      for row in Data.First_Index .. Data.Last_Index loop
         Row_Size := int (Data (row).Length);
         Tuple := PyTuple_New (Row_Size);
         Py_Row := Py_Row + 1;
         Py_Col := -1;
         for col in Data (row).First_Index .. Data (row).Last_Index loop
            Py_Col := Py_Col + 1;
            if Data (row) (col) then
               Long_Value := 1;
            else
               Long_Value := 0;
            end if;

            PyTuple_SetItem (Tuple, Py_Col, PyBool_FromLong (Long_Value));
         end loop;

         PyTuple_SetItem (Tuple_2D, Py_Row, Tuple);
      end loop;

      return Tuple_2D;

   end To_Tuple;

   --  -------------------------------------------------------------------------

   function To_Tuple (Data : ML_Types.Bounded_String_List) return PyObject is
      use Interfaces.C;
--        Routine_Name : constant String := "Python.To_Tuple Bounded_String_List ";
      Tuple        : PyObject;
      Py_Index     : int := -1;
   begin
      Tuple := PyTuple_New (int (Data.Length));
      for row in Data.First_Index .. Data.Last_Index loop
         Py_Index := Py_Index + 1;
         declare
            Text : constant char_array := To_C (Data (row));
            Item : constant PyObject := PyString_FromString (Text);
         begin
            PyTuple_SetItem (Tuple, Py_Index, Item);
         end;
      end loop;

      return Tuple;

   end To_Tuple;

   --  -------------------------------------------------------------------------

   function To_Tuple (Data : ML_Arrays_And_Matrices.Real_Float_Matrix) 
                      return PyObject is
      use Interfaces.C;
--        Routine_Name : constant String := "Python.To_Tuple Real_Float_Matrix ";
      Num_Cols     : constant Positive := Data'Length (2);
      Row_Size     : constant int := int (Num_Cols);
      Value        : Float;
      Item         : PyObject;
      Py_Row       : int := -1;
      Py_Col       : int := -1;
      Result       : constant PyObject := PyTuple_New (int (Data'Length));
   begin
      for row in Data'Range loop
         Item := PyTuple_New (Row_Size);
         Py_Row := Py_Row + 1;
         Py_Col := -1;
         for col in Data'Range (2) loop
            Py_Col := Py_Col + 1;
            Value := Data (row, col);
            PyTuple_SetItem (Item, Py_Col, PyFloat_FromDouble (double (Value)));
         end loop;
         PyTuple_SetItem (Result, Py_Row, Item);
      end loop;

      return Result;
      
   end To_Tuple;

   --  -------------------------------------------------------------------------

   function To_Tuple (Data : ML_Arrays_And_Matrices.Real_Float_Vector) 
                      return PyObject is
      use Interfaces.C;
--        Routine_Name : constant String := "Python.To_Tuple Real_Float_Vector ";
      Value        : double;
      Py_Row       : int := -1;
      Result       : constant PyObject := PyTuple_New (int (Data'Length));
   begin
      for row in Data'Range loop
         Py_Row := Py_Row + 1;
         Value := double (Data (row));
         PyTuple_SetItem (Result, Py_Row, PyFloat_FromDouble (Value));
      end loop;

      return Result;

   end To_Tuple;

   --  -------------------------------------------------------------------------

   function To_Tuple (Data : ML_Types.Unbounded_List) return PyObject is
      use Interfaces.C;
      use Ada.Strings.Unbounded;
--        Routine_Name : constant String := "Python.To_Tuple Unbounded_List ";
      Tuple        : PyObject;
      Py_Index     : int := -1;
   begin
      Tuple := PyTuple_New (int (Data.Length));
      for row in Data.First_Index .. Data.Last_Index loop
         Py_Index := Py_Index + 1;
         declare
            Text : constant char_array := To_C (To_String (Data (row)));
            Item : constant PyObject := PyString_FromString (Text);
         begin
            PyTuple_SetItem (Tuple, Py_Index, Item);
         end;
      end loop;

      return Tuple;

   end To_Tuple;

   --  -------------------------------------------------------------------------
 
end Tuple_Builder;
