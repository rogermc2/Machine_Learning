
with System;

with Interfaces.C;

with Ada.Assertions; use Ada.Assertions;
with Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;

--  with Basic_Printing; use Basic_Printing;
with ML_Types;
with Python_API;

package body Python_16A is

   function Parse_Dictionary_Record (Tuple : Python_API.PyObject_Ptr)
                                     return Support_16A.Dictionary_Record;
   function Parse_Tuples (Tuples : Python_API.PyObject_Ptr) return
     Support_16A.Newsgroups_Record;

   --  -------------------------------------------------------------------------

   function Call (M : Python.Module; Function_Name : String)
                  return Support_16A.Dictionary_Record is
      use Python_API;
      F        : constant PyObject_Ptr := Python.Get_Symbol (M, Function_Name);
      PyResult : PyObject_Ptr;
      Result   : Support_16A.Dictionary_Record;
   begin
      PyResult := Python.Call_Object (F);
      Py_DecRef (F);

      Result := Parse_Dictionary_Record (PyResult);
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

   function Parse_Dictionary_Record (Tuple : Python_API.PyObject_Ptr)
                                     return Support_16A.Dictionary_Record is
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
      Data_List       : Support_16A.Dictionary_Record;
   begin
      New_Line;
      Assert (Tuple /= System.Null_Address, Routine_Name & "Tuple is null.");
      --        Put_Line (Routine_Name & "Tuple_Size: " & int'Image (Tuple_Size));

      for item in 0 .. Tuple_Size - 1 loop
         Tuple_Item := PyTuple_GetItem (Tuple, item);
         Assert (Tuple_Item /= System.Null_Address, Routine_Name &
                   "Tuple_Item is null");
         Tuple_Item_Size := Integer (PyTuple_Size (Tuple_Item));
         --           Put_Line (Routine_Name & "Tuple_Item size: " &
         --                       Integer'Image (Tuple_Item_Size));

         declare
            Text          : String (1 .. Tuple_Item_Size);
            Has_Long_Char : Boolean := False;
         begin
            for index in 0 .. Tuple_Item_Size - 1 loop
               Py_Str_Ptr := PyTuple_GetItem (Tuple_Item, int (index));
               declare
                  aChar     : constant String :=
                                Python.Py_String_To_Ada (Py_Str_Ptr);
                  Long_Char : constant Boolean := aChar'Length > 1;
               begin
                  if Long_Char then
                     Has_Long_Char := True;
                     --                       Put_Line (Routine_Name & "Item, index" &
                     --                                   int'Image (item) & ","  &
                     --                                   Integer'Image (index) & ":");
                     --                       Put_Line ("String: " & aChar);
                     --                       Put_Line ("String (1): " & aChar (1));
                     Text (index + 1) := '|';
                  else
                     Text (index + 1) := aChar (1);
                  end if;
               end;

               if Has_Long_Char then
                  Put_Line (Text);
               end if;
            end loop;
            Data_List.Append (To_Unbounded_String (Text));
            --              Put_Line (Routine_Name & "Text: " & Text);
         end;

      end loop;

      return Data_List;

   end Parse_Dictionary_Record;

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
      --        Put_Line (Routine_Name & "Tuple_Size: " & int'Image (Tuple_Size));

      for item in 0 .. Tuple_Size - 1 loop
         Tuple_Item := PyTuple_GetItem (Tuple, item);
         Assert (Tuple_Item /= System.Null_Address, Routine_Name &
                   "Tuple_Item is null");
         Tuple_Item_Size := Integer (PyTuple_Size (Tuple_Item));
         --           Put_Line (Routine_Name & "Tuple_Item size: " &
         --                       Integer'Image (Tuple_Item_Size));

         declare
            Text          : String (1 .. Tuple_Item_Size);
            Has_Long_Char : Boolean := False;
         begin
            for index in 0 .. Tuple_Item_Size - 1 loop
               Py_Str_Ptr := PyTuple_GetItem (Tuple_Item, int (index));
               declare
                  aChar     : constant String :=
                                Python.Py_String_To_Ada (Py_Str_Ptr);
                  Long_Char : constant Boolean := aChar'Length > 1;
               begin
                  if Long_Char then
                     Has_Long_Char := True;
                     --                       Put_Line (Routine_Name & "Item, index" &
                     --                                   int'Image (item) & ","  &
                     --                                   Integer'Image (index) & ":");
                     --                       Put_Line ("String: " & aChar);
                     --                       Put_Line ("String (1): " & aChar (1));
                     Text (index + 1) := '|';
                  else
                     Text (index + 1) := aChar (1);
                  end if;
               end;

               if Has_Long_Char then
                  Put_Line (Text);
               end if;
            end loop;
            Data_List.Append (To_Unbounded_String (Text));
            --              Put_Line (Routine_Name & "Text: " & Text);
         end;

      end loop;

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
