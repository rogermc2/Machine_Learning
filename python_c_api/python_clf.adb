--  Based on inspirel_ada-python_demo

with System; use System;
with Interfaces.C;

with Ada.Assertions; use Ada.Assertions;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Unchecked_Conversion;

--  with Basic_Printing; use Basic_Printing;
with Tuple_Builder; use Tuple_Builder;

package body Python_CLF is

   --  -------------------------------------------------------------------------

   function Call (M : Python.Module; Function_Name : String; A : Positive)
                  return PyObject is
      use Interfaces.C;

      function Py_BuildValue (Format : char_array; A : int) return PyObject;
      pragma Import (C, Py_BuildValue, "Py_BuildValue");

      F        : constant PyObject := Python.Get_Symbol (M, Function_Name);
      PyParams : PyObject;
      PyResult : PyObject;
   begin
      PyParams := Py_BuildValue (To_C ("(i)"), int (A));
      PyResult := Python.Call_Object (F, PyParams);

      Py_DecRef (F);
      Py_DecRef (PyParams);

      return PyResult;

   end Call;

   -- --------------------------------------------------------------------------

   function Call (M   : Python.Module; Function_Name : String;
                  CLF : PyObject; A : Integer_Array_List)
                  return Integer_Array is
      use Interfaces.C;
      use Python;

      function Py_BuildValue (Format : Interfaces.C.char_array;
                              T1, T2 : PyObject)  return PyObject;
      pragma Import (C, Py_BuildValue, "Py_BuildValue");

      Routine_Name : constant String := "Python_CLF.Call 4 * Integer_Array_List ";
      PyFunc       : constant PyObject := Get_Symbol (M, Function_Name);
      A_Tuple      : constant PyObject := To_Tuple (A);
      PyParams     : PyObject;
      Py_Result    : PyObject;
   begin
      Assert (A_Tuple /= Null_Address, Routine_Name & "A_Tuple is null");

      PyParams :=
        Py_BuildValue (Interfaces.C.To_C ("OO"), CLF, A_Tuple);
      Assert (PyParams /= Null_Address, Routine_Name & "PyParams is null");

      Py_Result := Call_Object (PyFunc, PyParams);
      if Py_Result = System.Null_Address then
         Put (Routine_Name & "Py error message: ");
         PyErr_Print;
      end if;

      declare
         Tuple_Size : constant int := PyTuple_Size (Py_Result);
         Tuple_Item : PyObject;
         Result     : Integer_Array (1 .. Integer (Tuple_Size));
      begin
         for index in 0 .. Tuple_Size - 1 loop
            Tuple_Item := PyTuple_GetItem (Py_Result, index);
            Result (Integer (index) + 1) :=
              Integer (PyLong_AsLong (Tuple_Item));
         end loop;
         --        Result := PyParse_Tuple (Py_Result);

         Py_DecRef (PyFunc);
         Py_DecRef (A_Tuple);
         Py_DecRef (PyParams);
         Py_DecRef (Py_Result);
         return Result;
      end;

   end Call;

   --  -------------------------------------------------------------------------

   procedure Call (M   : Python.Module; Function_Name : String;
                   CLF : in out PyObject;
                   A   : ML_Arrays_And_Matrices.Integer_Array_List;
                   B   : ML_Arrays_And_Matrices.Integer_Array_List) is
      use Python;

      function Py_BuildValue (Format             : Interfaces.C.char_array;
                              T1, T2, T3         : PyObject)  return PyObject;
      pragma Import (C, Py_BuildValue, "Py_BuildValue");

      Routine_Name : constant String := "Python_CLF.Call 4 * Integer_Array_List ";
      PyFunc       : constant PyObject := Get_Symbol (M, Function_Name);
      A_Tuple      : constant PyObject := To_Tuple (A);
      B_Tuple      : constant PyObject := To_Tuple (B);
      PyParams     : PyObject;
   begin
      Assert (A_Tuple /= Null_Address, Routine_Name & "A_Tuple is null");
      Assert (B_Tuple /= Null_Address, Routine_Name & "B_Tuple is null");

      PyParams :=
        Py_BuildValue (Interfaces.C.To_C ("OOO"), CLF, A_Tuple, B_Tuple);
      Assert (PyParams /= Null_Address, Routine_Name & "PyParams is null");

      CLF := Call_Object (PyFunc, PyParams);
      if CLF = System.Null_Address then
         Put (Routine_Name & "Py error message: ");
         PyErr_Print;
      end if;

      Py_DecRef (PyFunc);
      Py_DecRef (A_Tuple);
      Py_DecRef (B_Tuple);
      Py_DecRef (PyParams);

   end Call;

   --  -------------------------------------------------------------------------

   procedure Call (M   : Python.Module; Function_Name : String;
                   CLF : in out PyObject;
                   A   : ML_Arrays_And_Matrices.Integer_Array_List;
                   B   : ML_Arrays_And_Matrices.Integer_Array_List;
                   C   : ML_Arrays_And_Matrices.Integer_Array_List;
                   D   : ML_Arrays_And_Matrices.Integer_Array_List) is
      use Python;

      function Py_BuildValue (Format             : Interfaces.C.char_array;
                              T1, T2, T3, T4, T5 : PyObject)  return PyObject;
      pragma Import (C, Py_BuildValue, "Py_BuildValue");

      Routine_Name : constant String := "Python_CLF.Call 4 * Integer_Array_List ";
      PyFunc       : constant PyObject := Get_Symbol (M, Function_Name);
      A_Tuple      : constant PyObject := To_Tuple (A);
      B_Tuple      : constant PyObject := To_Tuple (B);
      C_Tuple      : constant PyObject := To_Tuple (C);
      D_Tuple      : constant PyObject := To_Tuple (D);
      PyParams     : PyObject;
   begin
      Assert (A_Tuple /= Null_Address, Routine_Name & "A_Tuple is null");
      Assert (B_Tuple /= Null_Address, Routine_Name & "B_Tuple is null");
      Assert (C_Tuple /= Null_Address, Routine_Name & "C_Tuple is null");
      Assert (D_Tuple /= Null_Address, Routine_Name & "D_Tuple is null");

      PyParams :=
        Py_BuildValue (Interfaces.C.To_C ("OOOOO"), CLF,
                       A_Tuple, B_Tuple, C_Tuple, D_Tuple);
      Assert (PyParams /= Null_Address, Routine_Name & "PyParams is null");

      CLF := Call_Object (PyFunc, PyParams);
      if CLF = System.Null_Address then
         Put (Routine_Name & "Py error message: ");
         PyErr_Print;
      end if;

      Py_DecRef (PyFunc);
      Py_DecRef (A_Tuple);
      Py_DecRef (B_Tuple);
      Py_DecRef (C_Tuple);
      Py_DecRef (D_Tuple);
      Py_DecRef (PyParams);

   end Call;

   --  -------------------------------------------------------------------------

end Python_CLF;
