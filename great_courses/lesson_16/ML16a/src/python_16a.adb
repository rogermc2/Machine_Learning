
with System;
with System.Address_To_Access_Conversions;

with Interfaces.C;
with Interfaces.C.Pointers;
with Interfaces.C.Strings;

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

   function Parse_Text_Tuple (Tuple : Python_API.PyObject_Ptr)
                              return ML_Types.Unbounded_List is
      use System;
      use Interfaces.C;
      use Interfaces.C.Strings;
      use Ada.Strings.Unbounded;
      use ML_Types;
      use Python_API;
      Routine_Name : constant String := "Python_16A.Parse_Text_Tuple  ";
      Tuple_Size   : constant int := PyTuple_Size (Tuple);
      Tuple_Item   : PyObject_Ptr;
      Tuple_Item_Size : Integer;
      Py_Item      : PyObject_Ptr;
      --        aByte        : unsigned_char;
      aChar        : char;
      Value_Ptr    : chars_ptr;
      Text_Length  : size_t;
      Char_Ptr     : access char;
      --        aChar        : String (1 .. 1);
      --        Char_1       : String (1 .. 1);
      Text         : Unbounded_String;
      Data_List    : Unbounded_List;
      --        Done         : Boolean := False;
   begin
      Assert (Tuple /= System.Null_Address, Routine_Name & "Tuple is null.");
      Put_Line (Routine_Name & "Tuple_Size: " & int'Image (Tuple_Size));

      Tuple_Item := PyTuple_GetItem (Tuple, 0);
      Assert (Tuple_Item /= System.Null_Address, Routine_Name &
                "Tuple_Item is null");
      Tuple_Item_Size := Integer (PyTuple_Size (Tuple_Item));
      Put_Line (Routine_Name & "Tuple_Item size: " &
                  Integer'Image (Tuple_Item_Size));

      declare
         type Char_Array is array (int range <>) of aliased char;
         Text : String (1 .. Tuple_Item_Size);
         package Pointer_Arithmetic is new Interfaces.C.Pointers
           (int, char, Char_Array, nul);
         Chars : Char_Array (0 .. int (Tuple_Item_Size - 1));
         Var : Pointer_Arithmetic.Pointer :=
                 Chars (Chars'First)'access;
      begin
         for index in 0 .. Tuple_Item_Size - 1 loop
            Py_Item := PyTuple_GetItem (Tuple_Item, 0);
            Assert (Py_Item /= System.Null_Address, Routine_Name &
                      "Py_Item is null");
            Char_Ptr := Convert.To_Pointer (Tuple_Item);
            aChar := Char_Ptr.all;
            Put_Line (Routine_Name & "aChar: " & Character (aChar));
            Pointer_Arithmetic.Increment (Char_Ptr);
         end loop;
      end;

      --        for index in 0 .. PyTuple_Size (Tuple_Item) - 1 loop
      --           Put_Line (Routine_Name & "index" & int'Image (index));
      --           Text_Length := Strlen (Value_Ptr);
      --           Put_Line (Routine_Name & "Text_Length" & size_t'Image (Text_Length));
      --           Text := To_Unbounded_String (Value (Value_Ptr, Text_Length));
      --           Put_Line (Routine_Name & To_String (Text));
      --           Append (Text, aChar);
      --           while not Done loop
      --              if aChar /= "/" then
      --                 Append (Text, aChar);
      --                 Value_Ptr := Value_Ptr + 1;
      --                 aChar (1) := To_Ada (Value_Ptr(1));
      --              else
      --                 Value_Ptr := Value_Ptr + 1;
      --                 Char_1 (1) := To_Ada (Value_Ptr(1));
      --                 Done := Char_1 = "0";
      --                 if not Done then
      --                    Append (Text, aChar);
      --                    Append (Text, Char_1);
      --                    Value_Ptr := Value_Ptr + 1;
      --                    aChar (1) := To_Ada (Value_Ptr(1));
      --                 end if;
      --              end if;
      --        end loop;

      Data_List.Append (Text);
      Put_Line (Routine_Name & "data addedd to Data_List");
      --        end loop;

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
