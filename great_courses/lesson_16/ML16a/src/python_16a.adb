
with System;

with Interfaces.C;

with Ada.Assertions; use Ada.Assertions;
with Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;

--  with Basic_Printing; use Basic_Printing;
with ML_Types;

package body Python_16A is

   function Parse_Occurrences_Dictionary (Tuple : Python_API.PyObject_Ptr)
                                          return Support_16A.Occurrences_Dictionary;
   function Parse_Tuples (Tuples : Python_API.PyObject_Ptr) return
     Support_16A.Newsgroups_Record;

   --  -------------------------------------------------------------------------

   function Call (M         : Python.Module; Function_Name : String;
                  Tokeniser : Python_API.PyObject_Ptr)
                  return Support_16A.Occurrences_Dictionary is
      use Interfaces.C;
      use Python_API;

      function Py_BuildValue (Format : char_array; O1 : PyObject_Ptr)
                              return PyObject_Ptr;
      pragma Import (C, Py_BuildValue, "Py_BuildValue");

      F        : constant PyObject_Ptr := Python.Get_Symbol (M, Function_Name);
      PyParams : PyObject_Ptr;
      PyResult : PyObject_Ptr;
      Result   : Support_16A.Occurrences_Dictionary;
   begin
      PyParams := Py_BuildValue (To_C ("(O)"), Tokeniser);
      PyResult := Python.Call_Object (F, PyParams);
      Py_DecRef (F);
      Py_DecRef (PyParams);

      Result := Parse_Occurrences_Dictionary (PyResult);
      Py_DecRef (PyResult);

      return Result;

   end Call;

   -- --------------------------------------------------------------------------

   function Call (M : Python.Module; Function_Name : String)
                  return Support_16A.Newsgroups_Record is
      use Python_API;
      F        : constant PyObject_Ptr := Python.Get_Symbol (M, Function_Name);
      PyResult : PyObject_Ptr;
      Result   : Support_16A.Newsgroups_Record;
   begin
      PyResult := Python.Call_Object (F);
      Py_DecRef (F);

      Result := Parse_Tuples (PyResult);
      Py_DecRef (PyResult);

      return Result;

   end Call;

   -- --------------------------------------------------------------------------

   function Parse_Occurrences_Dictionary
     (Tuple : Python_API.PyObject_Ptr)
      return Support_16A.Occurrences_Dictionary is
      use System;
      use Interfaces.C;
      use Ada.Strings.Unbounded;
      use Python;
      use Python_API;
      use Support_16A.Occurrences_Dictionary_Package;
      Routine_Name    : constant String :=
                          "Python_16A.Parse_Occurrences_Dictionary ";
      Tuple_Size      : constant int := PyTuple_Size (Tuple);
      Item_Ptr        : PyObject_Ptr;
      Py_Str_Ptr      : PyObject_Ptr;
      Key             : Unbounded_String;
      Value           : Integer;
      Data            : Support_16A.Occurrences_Dictionary;
   begin
      New_Line;
      Assert (Tuple /= Null_Address, Routine_Name & "Tuple is null.");

      for item in 0 .. Tuple_Size - 1 loop
         Item_Ptr := PyTuple_GetItem (Tuple, item);
         Assert (Item_Ptr /= Null_Address, Routine_Name &
                   "Item_Ptr is null");
         Py_Str_Ptr := PyTuple_GetItem (Item_Ptr, 0);
         Assert (Py_Str_Ptr /= Null_Address, Routine_Name &
                   "Py_Str_Ptr is null.");

         Key := To_Unbounded_String (Py_String_To_Ada (Py_Str_Ptr));
         if Length (Key) > 0 then
--              Put_Line (Routine_Name & "item, Key: "& int'Image (item) &
--                          ", " &  To_String (Key));
            Value :=
              Integer (PyInt_AsLong (PyTuple_GetItem (Item_Ptr, 1)));
            Data.Insert (Key, Value);
         end if;
      end loop;

      Py_DecRef (Item_Ptr);
      Py_DecRef (Py_Str_Ptr);

      return Data;

   end Parse_Occurrences_Dictionary;

   -- --------------------------------------------------------------------------

   function Parse_Text_Tuple (Tuple : Python_API.PyObject_Ptr)
                              return ML_Types.Unbounded_List is
      use System;
      use Interfaces.C;
      use Ada.Strings.Unbounded;
      use ML_Types;
      use Python_API;
      Routine_Name    : constant String := "Python_16A.Parse_Text_Tuple ";
      Tuple_Size      : constant int := PyTuple_Size (Tuple);
      Tuple_Item      : PyObject_Ptr;
      Tuple_Item_Size : Integer;
      Py_Str_Ptr      : PyObject_Ptr;
      Data_List       : Unbounded_List;
   begin
      New_Line;
      Assert (Tuple /= System.Null_Address, Routine_Name & "Tuple is null.");

      for item in 0 .. Tuple_Size - 1 loop
         Tuple_Item := PyTuple_GetItem (Tuple, item);
         Assert (Tuple_Item /= System.Null_Address, Routine_Name &
                   "Tuple_Item is null");
         Tuple_Item_Size := Integer (PyTuple_Size (Tuple_Item));

         declare
--              Text          : String (1 .. Tuple_Item_Size);
            Text          : Unbounded_String;
            Has_Long_Char : Boolean := False;
            Count         : Natural := 0;
            Continue      : Boolean := True;
            Index         : Natural := 0;
         begin
--              for index in 0 .. Tuple_Item_Size - 1 loop
            while Continue and then index < Tuple_Item_Size loop
               Py_Str_Ptr := PyTuple_GetItem (Tuple_Item, int (index));
               declare
                  aChar     : constant String :=
                                Python.Py_String_To_Ada (Py_Str_Ptr);
                  Long_Char : constant Boolean := aChar'Length > 1;
               begin
                  Continue := aChar (1) /= '@' and
                    aChar (1) /= Character'Val (0);
                  if Long_Char then
                     Has_Long_Char := True;
                     Text :=  Text & "|";
                  elsif aChar'Length = 1 then
                     Text :=  Text & aChar (1);
                  end if;
               end;

               if Count < 10 and then Has_Long_Char then
                  Count := Count + 1;
                  Put_Line (To_String (Text));
               end if;
               Index := Index + 1;
            end loop;

            if Length (Text) > 0 then
               Data_List.Append (Text);
            end if;
         end;

      end loop;

      Py_DecRef (Tuple_Item);
      Py_DecRef (Py_Str_Ptr);

      return Data_List;

   end Parse_Text_Tuple;

   -- --------------------------------------------------------------------------

   function Parse_Tuples (Tuples : Python_API.PyObject_Ptr)
                          return Support_16A.Newsgroups_Record is
      use Interfaces.C;
      use Python_API;
      --        Routine_Name : constant String := "Python_16A.Parse_Tuples  ";
      --        Tuples_Size  : constant int := PyTuple_Size (Tuples);
      Result       : Support_16A.Newsgroups_Record;
   begin
      --        Put_Line (Routine_Name & "Tuple_Size" & int'Image (Tuples_Size));
      Result.Data := Parse_Text_Tuple (PyTuple_GetItem (Tuples, 0));
      --        Result.Target := PyTuple_GetItem (Tuple, 1);
      --        Result.File_Names := PyTuple_GetItem (Tuple, 2);
      --        Result.Descr := PyTuple_GetItem (Tuple, 3);
      --        Result.Target_Names := PyTuple_GetItem (Tuple, 4);

      return Result;

   end Parse_Tuples;

   --  ----------------------------------------------------------------------

end Python_16A;
