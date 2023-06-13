
--  Based on inspirel_ada-python_demo

with Interfaces.C;

with Ada.Exceptions; use Ada.Exceptions;
with Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Unchecked_Conversion;

package body Tuple_Builder is

   --  -------------------------------------------------------------------------

   function To_Tuple (Data : ML_Arrays_And_Matrices.Float_Array) 
                      return PyObject_Ptr is
      use Interfaces.C;
      Routine_Name : constant String := "Python.To_Tuple Float_Array ";
      Value        : Float;
      Py_Row       : int := -1;
      Result       : constant PyObject_Ptr := PyTuple_New (int (Data'Length));
   begin
      for row in Data'Range loop
         Py_Row := Py_Row + 1;
         Value := Data (row);
         PyTuple_SetItem (Result, Py_Row, PyFloat_FromDouble (double (Value)));
      end loop;

      return Result;

   exception
      when E : others =>
         Put_Line (Routine_Name & "error" & Exception_Message (E));
         raise;

   end To_Tuple;

   --  -------------------------------------------------------------------------

   function To_Tuple (Data : ML_Arrays_And_Matrices.Float_Array_3D) 
                      return PyObject_Ptr is
      use Interfaces.C;
      Routine_Name  : constant String := "Python.To_Tuple Float_Array_3D ";
      Result        : constant PyObject_Ptr := PyTuple_New (int (Data'Length));
      Py_Row        : constant PyObject_Ptr := PyTuple_New (int (Data'Length (2)));
      Py_Col        : constant PyObject_Ptr := PyTuple_New (int (Data'Length (3)));
      Py_Row_Index  : int := -1;
      Py_Col_Index  : int;
      Py_Vert_Index : int;
      Value         : Float;
   begin
      for row in Data'Range loop
         Py_Row_Index := Py_Row_Index + 1;
         Py_Col_Index := -1;
         for col in Data'Range (1) loop
            Py_Col_Index := Py_Col_Index + 1;
            Py_Vert_Index := -1;
            for vert in Data'Range (2) loop
               Py_Vert_Index := Py_Vert_Index + 1;
               Value := Data (row, col, vert);
               PyTuple_SetItem (Py_Col, Py_Vert_Index, PyFloat_FromDouble (double (Value)));
            end loop;
            PyTuple_SetItem (Py_Row, Py_Col_Index, Py_Col);
         end loop;
         PyTuple_SetItem (Result, Py_Row_Index, Py_Row);
      end loop;
      
      Py_DecRef (Py_Row);
      Py_DecRef (Py_Col);

      return Result;

   exception
      when E : others =>
         Put_Line (Routine_Name & "error" & Exception_Message (E));
         raise;

   end To_Tuple;

   --  -------------------------------------------------------------------------

   function To_Tuple (Data : ML_Arrays_And_Matrices.Integer_Array) 
                      return PyObject_Ptr is
      use Interfaces.C;
      Routine_Name : constant String := "Python.To_Tuple Integer_Matrix ";
      Value        : Integer;
      Py_Row       : int := -1;
      Result       : constant PyObject_Ptr := PyTuple_New (int (Data'Length));
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

   function To_Tuple (Data : ML_Arrays_And_Matrices.Float_Array_List)
                      return PyObject_Ptr is
      use Interfaces.C;
      use ML_Arrays_And_Matrices;
      
      function Py_BuildValue (Format : char_array; T1 : double)
                              return PyObject_Ptr;
      pragma Import (C, Py_BuildValue, "Py_BuildValue");
      
      --        Routine_Name : constant String := "Python.To_Tuple Integer_Array_List ";
      Tuple        : PyObject_Ptr;
      Py_Index     : int := -1;
   begin
      Tuple := PyTuple_New (int (Data.Length));
      for row in Data.First_Index .. Data.Last_Index loop
         Py_Index := Py_Index + 1;
         declare
            Row_Data     : constant Float_Array := Data (row);
            PyParams     : PyObject_Ptr;
            Row_Tuple    : PyObject_Ptr;
            Py_Row_Index : int := -1;
         begin
            Row_Tuple := PyTuple_New (int (Row_Data'Length));
            for index in Row_Data'Range loop
               Py_Row_Index := Py_Row_Index + 1;
               PyParams :=
                 Py_BuildValue (To_C ("(d)"), double (Row_Data (index)));
               PyTuple_SetItem (Row_Tuple, Py_Row_Index, PyParams); 
               Py_DecRef (PyParams);
            end loop;
            PyTuple_SetItem (Tuple, Py_Index, To_Tuple (Row_Data));
         end;
      end loop;

      return Tuple;

   end To_Tuple;

   --  -------------------------------------------------------------------------

   function To_Tuple (Data : ML_Arrays_And_Matrices.Float_Matrix_List)
                      return PyObject_Ptr is
      use Interfaces.C;
      use ML_Arrays_And_Matrices;
      
      --        Routine_Name : constant String := "Python.To_Tuple Float_Matrix_List ";
      Tuple        : PyObject_Ptr;
      Py_Index     : int := -1;
   begin
      Tuple := PyTuple_New (int (Data.Length));
      for mat in Data.First_Index .. Data.Last_Index loop
         Py_Index := Py_Index + 1;
         declare
            Row_Data     : constant Real_Float_Matrix := Data (mat);
         begin
            PyTuple_SetItem (Tuple, Py_Index, To_Tuple (Row_Data));
         end;
      end loop;

      return Tuple;

   end To_Tuple;

   --  -------------------------------------------------------------------------

   function To_Tuple (Data : ML_Arrays_And_Matrices.Integer_Array_List)
                      return PyObject_Ptr is
      use Interfaces.C;
      use ML_Arrays_And_Matrices;
      
      function Py_BuildValue (Format : char_array; T1 : int)
                              return PyObject_Ptr;
      pragma Import (C, Py_BuildValue, "Py_BuildValue");
      
      --        Routine_Name : constant String := "Python.To_Tuple Integer_Array_List ";
      Tuple        : PyObject_Ptr;
      Py_Index     : int := -1;
   begin
      Tuple := PyTuple_New (int (Data.Length));
      for row in Data.First_Index .. Data.Last_Index loop
         Py_Index := Py_Index + 1;
         declare
            Row_Data     : constant Integer_Array := Data (row);
            PyParams     : PyObject_Ptr;
            Row_Tuple    : PyObject_Ptr;
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

   function To_Tuple (Data : ML_Types.Integer_List) return PyObject_Ptr is
      use Interfaces.C;
      --        Routine_Name : constant String := "Python.To_Tuple Integer_List ";
      Tuple        : constant PyObject_Ptr := PyTuple_New (int (Data.Length));
      Value        : long;
      Py_Index     : int := -1;
   begin
      if not Data.Is_Empty then
         for index in Data.First_Index .. Data.Last_Index loop
            Py_Index := Py_Index + 1;
            Value := long (Data.Element (index));
            PyTuple_SetItem (Tuple, Py_Index, PyLong_FromLong (Value));
         end loop;
      end if;

      return Tuple;

   end To_Tuple;

   --  -------------------------------------------------------------------------

   function To_Tuple (Data : ML_Types.Integer_List_2D) 
                      return PyObject_Ptr is
      use Interfaces.C;
      --        Routine_Name : constant String := "Python.To_Tuple Integer_List_2D ";
      Row_Size     : int;
      Value        : Integer;
      Data_Row     : ML_Types.Integer_List;
      Item         : PyObject_Ptr;
      Py_Row       : int := -1;
      Py_Col       : int;
      Result       : constant PyObject_Ptr := PyTuple_New (int (Data.Length));
   begin
      if not Data.Is_Empty then
         for row in Data.First_Index .. Data.Last_Index loop
            Row_Size := int (Data.Element (row).Length);
            Item := PyTuple_New (Row_Size);
            Data_Row := Data (row);
            Py_Row := Py_Row + 1;
            Py_Col := -1;
            if not Data_Row.Is_Empty then
               for col in Data_Row.First_Index .. Data_Row.Last_Index loop
                  Py_Col := Py_Col + 1;
                  Value := Data_Row (col);
                  PyTuple_SetItem (Item, Py_Col,
                                   PyLong_FromLong (long (Value)));
               end loop;
            end if;
            PyTuple_SetItem (Result, Py_Row, Item);
         end loop;
      end if;
      
      Py_DecRef (Item);
      
      return Result;

   end To_Tuple;

   --  -------------------------------------------------------------------------

   function To_Tuple (Data : ML_Arrays_And_Matrices.Integer_Matrix) 
                      return PyObject_Ptr is
      use Interfaces.C;
      --        Routine_Name : constant String := "Python.To_Tuple Integer_Matrix ";
      Num_Cols     : constant Positive := Data'Length (2);
      Row_Size     : constant int := int (Num_Cols);
      Value        : Integer;
      Item         : PyObject_Ptr;
      Py_Row       : int := -1;
      Py_Col       : int := -1;
      Result       : constant PyObject_Ptr := PyTuple_New (int (Data'Length));
   begin
      for row in Data'Range loop
         Item := PyTuple_New (Row_Size);
         Py_Row := Py_Row + 1;
         Py_Col := -1;
         for col in Data'Range (2) loop
            Py_Col := Py_Col + 1;
            Value := Data (row, col);
            PyTuple_SetItem (Item, Py_Col, PyLong_FromLong (long (Value)));
         end loop;
         PyTuple_SetItem (Result, Py_Row, Item);
      end loop;
      
      return Result;

   end To_Tuple;

   --  -------------------------------------------------------------------------

   function To_Tuple (Data : ML_Arrays_And_Matrices.Boolean_Array) 
                      return PyObject_Ptr is
      use Interfaces.C;
      Routine_Name : constant String := "Python.To_Tuple Boolean_Array ";
      Value        : long;
      Py_Row       : int := -1;
      Result       : constant PyObject_Ptr := PyTuple_New (int (Data'Length));
   begin
      for row in Data'Range loop
         Py_Row := Py_Row + 1;
         if Data (row) then
            Value := long (1);
         else
            Value := long (0);
         end if;
         
         PyTuple_SetItem (Result, Py_Row, PyBool_FromLong (Value));
      end loop;

      return Result;

   exception
      when E : others =>
         Put_Line (Routine_Name & "error" & Exception_Message (E));
         raise;

   end To_Tuple;

   --  -------------------------------------------------------------------------

   function To_Tuple (Data : NL_Types.Boolean_List) return PyObject_Ptr is
      use Interfaces.C;
      --        Routine_Name : constant String := "Python.To_Tuple Boolean_List ";
      Tuple_2D     : constant PyObject_Ptr := PyTuple_New (int (Data.Length));
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

   function To_Tuple (Data : NL_Types.Boolean_List_2D) return PyObject_Ptr is
      use Interfaces.C;
      --        Routine_Name : constant String := "Python.To_Tuple Boolean_List_2D ";
      Row_Size     : int;
      Long_Value   : long;
      Tuple_2D     : PyObject_Ptr;
      Tuple        : PyObject_Ptr;
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

   function To_Tuple (Data : ML_Arrays_And_Matrices.Real_Float_List_2D)
                      return PyObject_Ptr is
      use Interfaces.C;
      --        Routine_Name : constant String := "Python.To_Tuple Real_Float_List_2D ";
      Row_Data     : ML_Arrays_And_Matrices.Real_Float_List;
      Row_Size     : int;
      Value        : Float;
      Tuple_2D     : PyObject_Ptr;
      Tuple        : PyObject_Ptr;
      Py_Row       : int := -1;
      Py_Col       : int := -1;
   begin
      Tuple_2D := PyTuple_New (int (Data.Length));
      for row in Data.First_Index .. Data.Last_Index loop
         Row_Data := Data (row);
         Row_Size := int (Row_Data.Length);
         Tuple := PyTuple_New (Row_Size);
         Py_Row := Py_Row + 1;
         Py_Col := -1;
         for col in Row_Data.First_Index .. Row_Data.Last_Index loop
            Py_Col := Py_Col + 1;
            Value := Row_Data (col);
            PyTuple_SetItem (Tuple, Py_Col,
                             PyFloat_FromDouble (double (Value)));
         end loop;

         PyTuple_SetItem (Tuple_2D, Py_Row, Tuple);
      end loop;

      return Tuple_2D;

   end To_Tuple;

   --  -------------------------------------------------------------------------

   function To_Tuple (Data : ML_Types.Bounded_String_List) return PyObject_Ptr is
      use Interfaces.C;
      --        Routine_Name : constant String := "Python.To_Tuple Bounded_String_List ";
      Tuple        : PyObject_Ptr;
      Py_Index     : int := -1;
   begin
      Tuple := PyTuple_New (int (Data.Length));
      for row in Data.First_Index .. Data.Last_Index loop
         Py_Index := Py_Index + 1;
         declare
            Text : constant char_array := To_C (Data (row));
            Item : constant PyObject_Ptr := PyString_FromString (Text);
         begin
            PyTuple_SetItem (Tuple, Py_Index, Item);
         end;
      end loop;

      return Tuple;

   end To_Tuple;

   --  -------------------------------------------------------------------------

   function To_Tuple (Data : NL_Types.Float_List) return PyObject_Ptr is
      use Interfaces.C;
      --        Routine_Name : constant String := "Python.To_Tuple Bounded_String_List ";
      Tuple        : PyObject_Ptr;
      Py_Index     : int := -1;
   begin
      Tuple := PyTuple_New (int (Data.Length));
      for index in Data.First_Index .. Data.Last_Index loop
         Py_Index := Py_Index + 1;
         declare
            Value : constant double := double (Data.Element (index));
            Item  : constant PyObject_Ptr := PyFloat_FromDouble (Value);
         begin
            PyTuple_SetItem (Tuple, Py_Index, Item);
         end;
      end loop;

      return Tuple;

   end To_Tuple;

   --  -------------------------------------------------------------------------

   function To_Tuple (Data : ML_Arrays_And_Matrices.Real_Float_List) 
                      return PyObject_Ptr is
      use Interfaces.C;
      --        Routine_Name : constant String := "Python.To_Tuple Real_Float_Vector ";
      Value        : double;
      Py_Row       : int := -1;
      Result       : constant PyObject_Ptr := PyTuple_New (int (Data.Length));
   begin
      for row in Data.First_Index .. Data.Last_Index loop
         Py_Row := Py_Row + 1;
         Value := double (Data.Element (row));
         PyTuple_SetItem (Result, Py_Row, PyFloat_FromDouble (Value));
      end loop;

      return Result;

   end To_Tuple;

   --  -------------------------------------------------------------------------

   function To_Tuple (Data : ML_Arrays_And_Matrices.Real_Float_Matrix) 
                      return PyObject_Ptr is
      use Interfaces.C;
      --        Routine_Name : constant String := "Python.To_Tuple Real_Float_Matrix ";
      Num_Cols     : constant Positive := Data'Length (2);
      Row_Size     : constant int := int (Num_Cols);
      Value        : Float;
      Item         : PyObject_Ptr;
      Py_Row       : int := -1;
      Py_Col       : int := -1;
      Result       : constant PyObject_Ptr := PyTuple_New (int (Data'Length));
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
                      return PyObject_Ptr is
      use Interfaces.C;
      --        Routine_Name : constant String := "Python.To_Tuple Real_Float_Vector ";
      Value        : double;
      Py_Row       : int := -1;
      Result       : constant PyObject_Ptr := PyTuple_New (int (Data'Length));
   begin
      for row in Data'Range loop
         Py_Row := Py_Row + 1;
         Value := double (Data (row));
         PyTuple_SetItem (Result, Py_Row, PyFloat_FromDouble (Value));
      end loop;

      return Result;

   end To_Tuple;

   --  -------------------------------------------------------------------------

   function To_Tuple (Data : ML_Types.Indef_String_List) return PyObject_Ptr is
      use Interfaces.C;
      use ML_Types.Indefinite_String_Package;
      --        Routine_Name : constant String := "Python.To_Tuple Unbounded_List ";
      Curs         : Cursor := Data.First;
      Tuple        : PyObject_Ptr;
      Item         : PyObject_Ptr;
      Py_Index     : int := -1;
   begin
      Tuple := PyTuple_New (int (Data.Length));
      while Has_Element (Curs) loop
         Py_Index := Py_Index + 1;
         Item := PyString_FromString (To_C (Element (Curs)));
         PyTuple_SetItem (Tuple, Py_Index, Item);
         Next (Curs);
      end loop;

      return Tuple;

   end To_Tuple;

   --  -------------------------------------------------------------------------
 
   function To_Tuple (Data : ML_Arrays_And_Matrices.Real_Vector_List)
                      return PyObject_Ptr is
      use Interfaces.C;
      use ML_Arrays_And_Matrices;
      
      function Py_BuildValue (Format : char_array; T1 : double) return PyObject_Ptr;
      pragma Import (C, Py_BuildValue, "Py_BuildValue");
      
      --    Routine_Name : constant String := "Python.To_Tuple Real_Vector_List ";
      Tuple        : PyObject_Ptr;
      Py_Index     : int := -1;
   begin
      Tuple := PyTuple_New (int (Data.Length));
      for row in Data.First_Index .. Data.Last_Index loop
         Py_Index := Py_Index + 1;
         declare
            Row_Data     : constant Real_Float_Vector := Data (row);
            PyParams     : PyObject_Ptr;
            Row_Tuple    : PyObject_Ptr;
            Py_Row_Index : int := -1;
         begin
            Row_Tuple := PyTuple_New (int (Row_Data'Length));
            for index in Row_Data'Range loop
               Py_Row_Index := Py_Row_Index + 1;
               PyParams :=
                 Py_BuildValue (To_C ("(d)"), double (Row_Data (index)));
               PyTuple_SetItem (Row_Tuple, Py_Row_Index, PyParams); 
               Py_DecRef (PyParams);
            end loop;
            PyTuple_SetItem (Tuple, Py_Index, To_Tuple (Row_Data));
         end;
      end loop;

      return Tuple;

   end To_Tuple;

   --  -------------------------------------------------------------------------

   function To_Tuple (Data : ML_Arrays_And_Matrices.Float_Vector_List)
                      return PyObject_Ptr is
      use Interfaces.C;
      use ML_Arrays_And_Matrices;
      
      function Py_BuildValue (Format : char_array; T1 : double)
                              return PyObject_Ptr;
      pragma Import (C, Py_BuildValue, "Py_BuildValue");
      
      --    Routine_Name : constant String := "Python.To_Tuple Float_Vector_List ";
      Tuple        : PyObject_Ptr;
      Py_Index     : int := -1;
   begin
      Tuple := PyTuple_New (int (Data.Length));
      for row in Data.First_Index .. Data.Last_Index loop
         Py_Index := Py_Index + 1;
         declare
            Row_Data     : constant Real_Float_Vector := Data (row);
            PyParams     : PyObject_Ptr;
            Row_Tuple    : PyObject_Ptr;
            Py_Row_Index : int := -1;
         begin
            Row_Tuple := PyTuple_New (int (Row_Data'Length));
            for index in Row_Data'Range loop
               Py_Row_Index := Py_Row_Index + 1;
               PyParams :=
                 Py_BuildValue (To_C ("(d)"), double (Row_Data (index)));
               PyTuple_SetItem (Row_Tuple, Py_Row_Index, PyParams); 
               Py_DecRef (PyParams);
            end loop;
            PyTuple_SetItem (Tuple, Py_Index, To_Tuple (Row_Data));
         end;
      end loop;

      return Tuple;

   end To_Tuple;

   --  -------------------------------------------------------------------------

   function To_Tuple (Data : ML_Types.Unbounded_List) return PyObject_Ptr is
      use Interfaces.C;
      use Ada.Strings.Unbounded;
      --        Routine_Name : constant String := "Python.To_Tuple Unbounded_List ";
      Tuple        : PyObject_Ptr;
      Py_Index     : int := -1;
   begin
      Tuple := PyTuple_New (int (Data.Length));
      for row in Data.First_Index .. Data.Last_Index loop
         Py_Index := Py_Index + 1;
         declare
            Text : constant char_array := To_C (To_String (Data (row)));
            Item : constant PyObject_Ptr := PyString_FromString (Text);
         begin
            PyTuple_SetItem (Tuple, Py_Index, Item);
         end;
      end loop;

      return Tuple;

   end To_Tuple;

   --  -------------------------------------------------------------------------
 
   function To_Tuple (Data : ML_Arrays_And_Matrices.Unbounded_String_Array)
                      return PyObject_Ptr is
      use Interfaces.C;
      use Ada.Strings.Unbounded;
      --        Routine_Name : constant String := "Python.To_Tuple Unbounded_List ";
      Tuple        : PyObject_Ptr;
      Py_Index     : int := -1;
   begin
      Tuple := PyTuple_New (int (Data'Length));
      for row in Data'Range loop
         Py_Index := Py_Index + 1;
         declare
            Text : constant char_array := To_C (To_String (Data (row)));
            Item : constant PyObject_Ptr := PyString_FromString (Text);
         begin
            PyTuple_SetItem (Tuple, Py_Index, Item);
         end;
      end loop;

      return Tuple;

   end To_Tuple;

   --  -------------------------------------------------------------------------
 
   function To_Tuple (Data : ML_Arrays_And_Matrices.Unbounded_String_Matrix) 
                      return PyObject_Ptr is
      use Interfaces.C;
      use Ada.Strings.Unbounded;
      --        Routine_Name : constant String := "Python.To_Tuple Unbounded_String_Matrix ";
      Num_Cols     : constant Positive := Data'Length (2);
      Row_Size     : constant int := int (Num_Cols);
      Row_Item     : PyObject_Ptr;
      Py_Row       : int := -1;
      Py_Col       : int;
      Result       : constant PyObject_Ptr := PyTuple_New (int (Data'Length));
   begin
      for row in Data'Range loop
         Row_Item := PyTuple_New (Row_Size);
         Py_Row := Py_Row + 1;
         Py_Col := -1;
         for col in Data'Range (2) loop
            Py_Col := Py_Col + 1;
            declare
               Text : constant char_array := To_C (To_String (Data (row, col)));
               Item : constant PyObject_Ptr := PyString_FromString (Text);
            begin
               PyTuple_SetItem (Row_Item, Py_Col, Item);
            end;
         end loop;
         PyTuple_SetItem (Result, Py_Row, Row_Item);
      end loop;

      return Result;
      
   end To_Tuple;

   --  -------------------------------------------------------------------------

end Tuple_Builder;
