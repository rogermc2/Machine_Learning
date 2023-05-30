
with System;

with Interfaces.C;

with Ada.Assertions; use Ada.Assertions;
with Ada.Exceptions; use Ada.Exceptions;
with Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;

--  with Basic_Printing; use Basic_Printing;
with ML_Types;
with Python_API;
with Python_CLF;

package body Python_16A is

   function Parse_Tuple (Tuple : Python_API.PyObject_Ptr) return Support_16A.Newsgroups_Record;

   --  -------------------------------------------------------------------------

   function Call_Object (PyFunc : Python_API.PyObject_Ptr)
                         return Python_API.PyObject_Ptr is
      use Interfaces.C;
      --        use Ada.Assertions;
      use type System.Address;
      use Python_API;
      Routine_Name : constant String := "Support_16A.Call_Object ";
      PyParams     : PyObject_Ptr;
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
      PyParams : PyObject_Ptr;
      PyResult : PyObject_Ptr;
      Result   : Support_16A.Newsgroups_Record;
   begin
      --        PyParams := Py_BuildValue (To_C ("(O)"), A);
      --        PyResult := Python.Call_Object (F, PyParams);
      PyResult := Call_Object (F);

      Py_DecRef (F);
      --        Py_DecRef (PyParams);

      Result := Parse_Tuple (PyResult);

      Py_DecRef (PyResult);
      return Result;

   end Call;

   -- --------------------------------------------------------------------------

   function Parse_Text_Tuple (Tuple : Python_API.PyObject_Ptr)
                              return ML_Types.Unbounded_List is
      use System;
      use Interfaces.C;
      use Ada.Strings.Unbounded;
      use ML_Types;
      use Python_API;
      Routine_Name : constant String := "Python_16A.Parse_Text_Tuple  ";
      Tuple_Size   : constant int := PyTuple_Size (Tuple);
      Tuple_Item   : PyObject_Ptr;
      Value_Ptr    : access char_array;
      aChar        : String (1 .. 1);
      Char_1       : String (1 .. 1);
      Text         : Unbounded_String;
      Data_List    : Unbounded_List;
      Done         : Boolean := False;
   begin
      Assert (Tuple /= System.Null_Address, Routine_Name & "Tuple is null.");
      Put_Line (Routine_Name & "Tuple_Size" & int'Image (Tuple_Size));
      for index in 0 .. Tuple_Size - 1 loop
         Put_Line (Routine_Name & "index" & int'Image (index));
         Tuple_Item := PyTuple_GetItem (Tuple, index);
         Put_Line (Routine_Name & "Tuple_Item set");
         Value_Ptr := PyBytes_AsString (Tuple_Item);
         Put_Line (Routine_Name & "Value_Ptr set");
         aChar (1) := To_Ada (Value_Ptr(1));
         Append (Text, aChar);
         while not Done loop
            if aChar /= "/" then
               Append (Text, aChar);
               Value_Ptr := Value_Ptr + 1;
               aChar (1) := To_Ada (Value_Ptr(1));
            else
               Value_Ptr := Value_Ptr + 1;
               Char_1 (1) := To_Ada (Value_Ptr(1));
               Done := Char_1 = "0";
               if not Done then
                  Append (Text, aChar);
                  Append (Text, Char_1);
                  Value_Ptr := Value_Ptr + 1;
                  aChar (1) := To_Ada (Value_Ptr(1));
               end if;
            end if;
         end loop;

         Data_List.Append (Text);
         Put_Line (Routine_Name & "data addedd to Data_List");
      end loop;

      return Data_List;

   end Parse_Text_Tuple;

   -- --------------------------------------------------------------------------

   function Parse_Tuple (Tuple : Python_API.PyObject_Ptr)
                         return Support_16A.Newsgroups_Record is
      use Interfaces.C;
      use Python_API;
      Routine_Name : constant String := "Python_16A.Parse_Tuple  ";
      Tuple_Size   : constant int := PyTuple_Size (Tuple);
      Result       : Support_16A.Newsgroups_Record;
   begin
      Put_Line (Routine_Name & "Tuple_Size" & int'Image (Tuple_Size));
      Result.Data := Parse_Text_Tuple (PyTuple_GetItem (Tuple, 0));
      Result.Target := PyTuple_GetItem (Tuple, 1);
--        Result.File_Names := PyTuple_GetItem (Tuple, 2);
--        Result.Descr := PyTuple_GetItem (Tuple, 3);
--        Result.Target_Names := PyTuple_GetItem (Tuple, 4);

      return Result;

   end Parse_Tuple;

   --  ----------------------------------------------------------------------

end Python_16A;
