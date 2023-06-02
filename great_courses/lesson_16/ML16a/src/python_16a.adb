
with System;
with System.Address_To_Access_Conversions;

with Interfaces.C;
--  with Interfaces.C.Pointers;
--  with Interfaces.C.Strings;

with Ada.Assertions; use Ada.Assertions;
with Ada.Exceptions; use Ada.Exceptions;
with Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;

--  with Basic_Printing; use Basic_Printing;
with ML_Types;
--  with ML_U8_Types;
with Python_API;
with Python_CLF;

package body Python_16A is

   package Convert is new System.Address_To_Access_Conversions
     (Interfaces.C.char);

   function Parse_Tuples (Tuples : Python_API.PyObject_Ptr) return
     Support_16A.Newsgroups_Record;

   --  -------------------------------------------------------------------------

   function Call_Object (PyFunc : Python_API.PyObject_Ptr)
                         return Python_API.PyObject_Ptr is
      use Interfaces.C;
      --        use Ada.Assertions;
      use type System.Address;
      use Python_API;
      Routine_Name : constant String := "Support_16A.Call_Object ";
      PyParams     : constant PyObject_Ptr := System.Null_Address;
      PyResult     : PyObject_Ptr;
   begin
      Assert (PyFunc /= System.Null_Address, Routine_Name & "PyFunc is null.");
      Assert (PyCallable_Check (PyFunc) /= 0, Routine_Name &
                "PyCallable_Check is null.");
      PyResult := PyObject_CallObject (PyFunc, PyParams);

      if PyResult = System.Null_Address then
         New_Line;
         Put_Line (Routine_Name & "Python error message:");
         PyErr_Print;
         raise Python_CLF.Interpreter_Error with Routine_Name & "failed.";
      end if;

      return PyResult;

   exception
      when E : others =>
         raise Python_CLF.Interpreter_Error with Routine_Name & "exception: " &
           Exception_Message (E);

   end Call_Object;

   --  -------------------------------------------------------------------------

   function Call (M : Python.Module; Function_Name : String)
                  return Support_16A.Newsgroups_Record is
      use Python_API;

      --        function Py_BuildValue (Format : char_array; A : PyObject_Ptr)
      --                                return PyObject_Ptr;
      --        pragma Import (C, Py_BuildValue, "Py_BuildValue");

      F        : constant PyObject_Ptr := Python.Get_Symbol (M, Function_Name);
      --        PyParams : PyObject_Ptr;
      PyResult : PyObject_Ptr;
      Result   : Support_16A.Newsgroups_Record;
   begin
      --        PyParams := Py_BuildValue (To_C ("(O)"), A);
      --        PyResult := Python.Call_Object (F, PyParams);
      PyResult := Call_Object (F);

      Py_DecRef (F);
      --        Py_DecRef (PyParams);

      Result := Parse_Tuples (PyResult);

      Py_DecRef (PyResult);
      return Result;

   end Call;

   -- --------------------------------------------------------------------------

   function C_To_Ada_String (C_String_Ptr : Python_API.PyObject_Ptr) return String is
      Routine_Name    : constant String := "Python_16A.C_To_Ada_String  ";

      procedure Move_Bytes( dst, src : Python_API.PyObject_Ptr; count : Positive );
      pragma Interface (C, Move_Bytes );
      pragma Interface_Name (Move_Bytes, "memcpy" );

      function Strlen (C_String_Ptr : in Python_API.PyObject_Ptr ) return Natural;
      pragma Interface (C, Strlen );
      pragma Interface_Name (Strlen, "strlen" );

      Length : constant Natural := Strlen (C_String_Ptr);
   begin
      Put_Line (Routine_Name & "Length: " & Integer'Image (Length));
      if Length < 1 then
         return "";
      else
         declare
            Ada_String : String (1..Length);
         begin
            Move_Bytes (Ada_String(1)'address, C_String_Ptr, Length);
            Put_Line (Routine_Name & "Ada_String (1): '" & Ada_String (1) &"'");
            return Ada_String;
         end;
      end if;

   end C_To_Ada_String;

   -- --------------------------------------------------------------------------

   function Parse_Text_Tuple (Tuple : Python_API.PyObject_Ptr)
                              return ML_Types.Unbounded_List is
      use System;
      use Interfaces.C;
      use Ada.Strings.Unbounded;
      use ML_Types;
      use Python_API;
      Routine_Name    : constant String := "Python_16A.Parse_Text_Tuple  ";
      Tuple_Size      : constant int := PyTuple_Size (Tuple);
      --        Char_Array_Ptr : Interfaces.C.Strings.char_array_access;
      Tuple_Item      : PyObject_Ptr;
      Tuple_Item_Size : Integer;
      Py_Str_Ptr      : PyObject_Ptr;
      Py_Str          : PyObject_Ptr;
      aByte           : char;
      Char_Ptr        : access char;
      Text            : Unbounded_String;
      Data_List       : Unbounded_List;
   begin
      New_Line;
      Assert (Tuple /= System.Null_Address, Routine_Name & "Tuple is null.");
      Put_Line (Routine_Name & "Tuple_Size: " & int'Image (Tuple_Size));

      Tuple_Item := PyTuple_GetItem (Tuple, 0);
      Assert (Tuple_Item /= System.Null_Address, Routine_Name &
                "Tuple_Item is null");
      Tuple_Item_Size := Integer (PyTuple_Size (Tuple_Item));
      Put_Line (Routine_Name & "Tuple_Item size: " &
                  Integer'Image (Tuple_Item_Size));

      declare
         Text  : String (1 .. Tuple_Item_Size);
      begin
         for index in 0 .. Tuple_Item_Size - 1 loop
            Py_Str_Ptr := PyTuple_GetItem (Tuple_Item, int (index));
            Assert (Py_Str_Ptr /= System.Null_Address, Routine_Name &
                      "Py_Str_Ptr is null");
            declare
               aChar: String := C_To_Ada_String (Py_Str_Ptr);
            begin
               Put_Line (Routine_Name & "aChar: " & aChar);
            end;

            --  Py_Str_Ptr size = 1
            --  Indexing a C string produces strings of length 1.
            Py_Str := PyObject_String (Py_Str_Ptr);
            Char_Ptr := Convert.To_Pointer (Py_Str);
            aByte := Char_Ptr.all;
            Text (index + 1) := To_Ada (aByte);
            Put_Line (Routine_Name & "Text (index + 1): " & Text (index + 1));
            --              Pointer_Arithmetic.Increment (Char_Ptr);
         end loop;
      end;

      Data_List.Append (Text);
      Put_Line (Routine_Name & "data addedd to Data_List");

      return Data_List;

   end Parse_Text_Tuple;

   -- --------------------------------------------------------------------------

   function Parse_Tuples (Tuples : Python_API.PyObject_Ptr)
                          return Support_16A.Newsgroups_Record is
      use Interfaces.C;
      use Python_API;
      Routine_Name : constant String := "Python_16A.Parse_Tuples  ";
      Tuples_Size  : constant int := PyTuple_Size (Tuples);
      Result       : Support_16A.Newsgroups_Record;
   begin
      Put_Line (Routine_Name & "Tuple_Size" & int'Image (Tuples_Size));
      Result.Data := Parse_Text_Tuple (PyTuple_GetItem (Tuples, 0));
      --        Result.Target := PyTuple_GetItem (Tuple, 1);
      --        Result.File_Names := PyTuple_GetItem (Tuple, 2);
      --        Result.Descr := PyTuple_GetItem (Tuple, 3);
      --        Result.Target_Names := PyTuple_GetItem (Tuple, 4);

      return Result;

   end Parse_Tuples;

   --  ----------------------------------------------------------------------

end Python_16A;
