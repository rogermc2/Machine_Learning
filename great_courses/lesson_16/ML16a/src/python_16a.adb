
with System;

with Interfaces.C;

with Ada.Assertions; use Ada.Assertions;
with Ada.Exceptions; use Ada.Exceptions;
with Ada.Text_IO; use Ada.Text_IO;

--  with Basic_Printing; use Basic_Printing;
with Python_API;
with Python_CLF;

package body Python_16A is

   --  -------------------------------------------------------------------------

   function Call_Object (PyFunc : Python_API.PyObject)
                         return Python_API.PyObject is
      use Interfaces.C;
--        use Ada.Assertions;
      use type System.Address;
      use Python_API;
      Routine_Name : constant String := "Support_16A.Call_Object ";
      PyParams     : PyObject;
      PyResult     : PyObject;
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

   function Call_Python (M : Python.Module; Function_Name : String)
                         return Support_16A.Newsgroups_Record is
      use Interfaces.C;
      use Python_API;

      --        function Py_BuildValue (Format : char_array; A : PyObject)
      --                                return PyObject;
      --        pragma Import (C, Py_BuildValue, "Py_BuildValue");

      function Parse_Tuple (Tuple : PyObject) return Support_16A.Newsgroups_Record is
         --           use ML_Types;
         --           Routine_Name : constant String := "Parsers.Parse_Tuple IL2D  ";
         --           Tuple_Size     : constant int := PyTuple_Size (Tuple);
         --           Tuple_Row_Size : constant int := PyTuple_Size (PyTuple_GetItem (Tuple, 1));
         --           Tuple_Row      : PyObject;
         --           Tuple_Item     : PyObject;
         --           Result_Row     : Integer_List;
         --           Value          : Integer;
         Result         : Support_16A.Newsgroups_Record;
      begin
         Result.Data := PyTuple_GetItem (Tuple, 0);
         Result.Target := PyTuple_GetItem (Tuple, 1);
         Result.File_Names := PyTuple_GetItem (Tuple, 2);
         Result.Descr := PyTuple_GetItem (Tuple, 3);
         Result.Target_Names := PyTuple_GetItem (Tuple, 4);
         --           for row in 0 .. Tuple_Size - 1 loop
         --              Tuple_Row := PyTuple_GetItem (Tuple, row);
         --              Result_Row.Clear;
         --              for col in 0 .. Tuple_Row_Size - 1 loop
         --                 Tuple_Item := PyTuple_GetItem (Tuple_Row, col);
         --                 Value := Integer (PyLong_AsLong (Tuple_Item));
         --                 Result_Row.Append (Value);
         --              end loop;

         --              Result.Append (Result_Row);
         --           end loop;

         return Result;

      end Parse_Tuple;

      --  ----------------------------------------------------------------------

      F        : constant PyObject := Python.Get_Symbol (M, Function_Name);
      PyParams : PyObject;
      PyResult : PyObject;
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

   end Call_Python;

   -- --------------------------------------------------------------------------

end Python_16A;
