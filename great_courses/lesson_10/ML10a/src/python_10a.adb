
with System;

with Interfaces.C;

with Ada.Assertions; use Ada.Assertions;
with Ada.Text_IO; use Ada.Text_IO;

--  with Basic_Printing; use Basic_Printing;
with Tuple_Builder;

package body Python_10A is

   function To_Array_Tuple (Data : Support_10A.Features_Array) return Python_API.PyObject;
   function To_Features_Tuple (Data : Support_10A.Features_Record)
                               return Python_API.PyObject;

   --  -------------------------------------------------------------------------

   function Call (M   : Python.Module; Function_Name : String;
                  CLF : Python_API.PyObject; A : Support_10A.Features_Array)
                  return Integer_Array is
      use System;
      use Interfaces.C;
      use Python;
      use Python_API;

      function Py_BuildValue (Format : char_array; O1, O2 : PyObject)
                              return PyObject;
      pragma Import (C, Py_BuildValue, "Py_BuildValue");

      procedure Parse_Tuple (Tuple : PyObject; Vec : in out Integer_Array) is
      begin
         Assert (Vec'Length = Integer (PyTuple_Size (Tuple)),
                 "Parse_Tuple Real_Float_List Tuple Size" &
                   int'Image (PyTuple_Size (Tuple))
                 & " /= Vec Length" & Integer'Image (Vec'Length));

         for index in 1 .. PyTuple_Size (Tuple) loop
            Vec (Integer (index)) :=
              Integer (PyInt_AsLong (PyTuple_GetItem (Tuple, index - 1)));
         end loop;

      end Parse_Tuple;

      Routine_Name : constant String := "Support_10A.Call ";
      F            : constant PyObject := Get_Symbol (M, Function_Name);
      A_Tuple      : constant PyObject := To_Array_Tuple (A);
      Py_Params    : PyObject;
      Py_Result    : PyObject;
      Result       : Integer_Array (A'Range);
   begin
      Assert (A_Tuple /= Null_Address, Routine_Name & "A_Tuple is null");
      Py_Params :=  Py_BuildValue (To_C ("OO"), CLF, A_Tuple);
      Py_Result := Call_Object (F, Py_Params);
      if Py_Result = System.Null_Address then
         Put (Routine_Name & "Py error message: ");
         PyErr_Print;
      end if;

      Parse_Tuple (Py_Result, Result);

      Py_DecRef (F);
      Py_DecRef (A_Tuple);
      Py_DecRef (Py_Params);
      Py_DecRef (Py_Result);

      return Result;

   end Call;

   -- --------------------------------------------------------------------------

   function Call (M   : Python.Module; Function_Name : String;
                  CLF : Python_API.PyObject; A : Support_10A.Features_Record)
                  return Real_Float_Vector is
      use System;
      use Interfaces.C;
      use Python;
      use Python_API;

      function Py_BuildValue (Format : char_array; O1, O2 : PyObject)
                              return PyObject;
      pragma Import (C, Py_BuildValue, "Py_BuildValue");

      function Parse_Tuple (Tuple : PyObject) return Real_Float_Vector is
      begin
         declare
            Vec : Real_Float_Vector (1 .. Integer (PyTuple_Size (Tuple)));
         begin
            for index in Vec'Range loop
               Vec (Integer (index)) :=
                 Float (PyFloat_AsDouble (PyTuple_GetItem
                        (Tuple, int (index - 1))));
            end loop;

            return Vec;
         end;

      end Parse_Tuple;

      Routine_Name : constant String := "Support_10A.Call ";
      F            : constant PyObject := Get_Symbol (M, Function_Name);
      A_Tuple      : constant PyObject := To_Features_Tuple (A);
      Py_Params    : PyObject;
      Py_Result    : PyObject;
   begin
      Assert (A_Tuple /= Null_Address, Routine_Name & "A_Tuple is null");
      Py_Params :=  Py_BuildValue (To_C ("OO"), CLF, A_Tuple);
      Py_Result := Call_Object (F, Py_Params);
      if Py_Result = System.Null_Address then
         Put (Routine_Name & "Py error message: ");
         PyErr_Print;
      end if;

      Py_DecRef (F);
      Py_DecRef (A_Tuple);
      Py_DecRef (Py_Params);

      declare
         Result : constant Real_Float_Vector := Parse_Tuple (Py_Result);
      begin
         Py_DecRef (Py_Result);
         return Result;
      end;

   end Call;

   -- --------------------------------------------------------------------------

   procedure Call (M   : Python.Module; Function_Name : String;
                   CLF : Python_API.PyObject; A : Support_10A.Features_Array;
                   B   : Integer_Array) is
      use System;
      use Interfaces.C;
      use Python;
      use Python_API;

      function Py_BuildValue (Format : char_array; O1, O2, O3 : PyObject)
                              return PyObject;
      pragma Import (C, Py_BuildValue, "Py_BuildValue");

      Routine_Name : constant String := "Support_10A.Call ";
      F            : constant PyObject := Get_Symbol (M, Function_Name);
      A_Tuple      : constant PyObject := To_Array_Tuple (A);
      B_Tuple      : constant PyObject := Tuple_Builder.To_Tuple (B);
      Py_Params    : PyObject;
      Py_Result    : PyObject;
   begin
      Assert (A_Tuple /= Null_Address, Routine_Name & "A_Tuple is null");
      Assert (B_Tuple /= Null_Address, Routine_Name & "B_Tuple is null");

      Py_Params :=  Py_BuildValue (To_C ("OOO"), CLF, A_Tuple, B_Tuple);
      Py_Result := Call_Object (F, Py_Params);
      if Py_Result = System.Null_Address then
         Put (Routine_Name & "Py error message: ");
         PyErr_Print;
      end if;

      Py_DecRef (F);
      Py_DecRef (A_Tuple);
      Py_DecRef (B_Tuple);
      Py_DecRef (Py_Params);
      Py_DecRef (Py_Result);

   end Call;

   -- --------------------------------------------------------------------------

   function To_Array_Tuple (Data : Support_10A.Features_Array)
                            return Python_API.PyObject is
      use Interfaces.C;
      use Python_API;
      --        Routine_Name : constant String := "Support_10A.To_Array_Tuple ";
      Py_Row       : int := -1;
      Result       : constant PyObject := PyTuple_New (int (Data'Length));
   begin
      for row in Data'Range loop
         Py_Row := Py_Row + 1;
         PyTuple_SetItem (Result, Py_Row, To_Features_Tuple (Data (row)));
      end loop;

      return Result;

   end To_Array_Tuple;

   --  -------------------------------------------------------------------------

   function To_Features_Tuple (Data : Support_10A.Features_Record)
                               return Python_API.PyObject is
      use Interfaces.C;
      use Python_API;
      --        Routine_Name : constant String := "Support_10A.To_Features_Tuple ";
      Result       : constant PyObject := PyTuple_New (9);
      Embark       : long;
   begin
      PyTuple_SetItem (Result, 0, PyLong_FromLong (long (Data.P_Class)));
      PyTuple_SetItem (Result, 1, PyLong_FromLong (long (Data.Sex)));
      PyTuple_SetItem (Result, 2, PyFloat_FromDouble (double (Data.Age)));
      PyTuple_SetItem (Result, 3, PyLong_FromLong (long (Data.Sib_Sp)));
      PyTuple_SetItem (Result, 4, PyLong_FromLong (long (Data.Parch)));
      PyTuple_SetItem (Result, 5, PyFloat_FromDouble (double (Data.Fare)));
      if Data.Embark_S then
         Embark := 1;
      else
         Embark := 0;
      end if;
      PyTuple_SetItem (Result, 6, PyBool_FromLong (Embark));

      if Data.Embark_C then
         Embark := 1;
      else
         Embark := 0;
      end if;
      PyTuple_SetItem (Result, 7, PyBool_FromLong (Embark));
      if Data.Embark_Q then
         Embark := 1;
      else
         Embark := 0;
      end if;
      PyTuple_SetItem (Result, 8, PyBool_FromLong (Embark));

      return Result;

   end To_Features_Tuple;

   --  -------------------------------------------------------------------------

end Python_10A;
