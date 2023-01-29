
--  Based on inspirel_ada-python_demo

with Interfaces.C;

with Ada.Assertions; use Ada.Assertions;
with Ada.Exceptions; use Ada.Exceptions;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Unchecked_Conversion;

--  with Basic_Printing; use Basic_Printing;

package body Python_CLF is

   --  -------------------------------------------------------------------------

   procedure Call (M : Module; Function_Name : String;
                   CLF : in out PyObject;
                   A : ML_Arrays_And_Matrices.Integer_Array_List;
                   B : ML_Arrays_And_Matrices.Integer_Array_List;
                   C : ML_Arrays_And_Matrices.Integer_Array_List;
                   D : ML_Arrays_And_Matrices.Integer_Array_List);
      use System;
      function Py_BuildValue (Format         : Interfaces.C.char_array;
                              T1, T2, T3, T4 : PyObject)  return PyObject;
      pragma Import (C, Py_BuildValue, "Py_BuildValue");

      Routine_Name : constant String := "Python.Call 4 * Integer_Array_List ";
      PyFunc       : constant PyObject := Get_Symbol (M, Function_Name);
      A_Tuple      : constant PyObject := To_Tuple (A);
      B_Tuple      : constant PyObject := To_Tuple (B);
      C_Tuple      : constant PyObject := To_Tuple (C);
      D_Tuple      : constant PyObject := To_Tuple (D);
      PyParams     : PyObject;
      PyResult     : PyObject;
      Result       : aliased Interfaces.C.double;
   begin
      Assert (A_Tuple /= Null_Address, Routine_Name & "A_Tuple is null");
      Assert (B_Tuple /= Null_Address, Routine_Name & "B_Tuple is null");
      Assert (C_Tuple /= Null_Address, Routine_Name & "C_Tuple is null");
      Assert (D_Tuple /= Null_Address, Routine_Name & "D_Tuple is null");

      PyParams :=
        Py_BuildValue (Interfaces.C.To_C ("OOOO"),
                       A_Tuple, B_Tuple, C_Tuple, D_Tuple);
      Assert (PyParams /= Null_Address, Routine_Name & "PyParams is null");

      PyResult := Call_Object (PyFunc, PyParams);
      if PyResult = System.Null_Address then
         Put (Routine_Name & "Py error message: ");
         PyErr_Print;
      end if;

      Result := PyFloat_AsDouble (PyResult);
--        Put_Line (Routine_Name & " Result: " &
--                    Interfaces.C.double'Image (Result));

      Py_DecRef (PyFunc);
      Py_DecRef (A_Tuple);
      Py_DecRef (B_Tuple);
      Py_DecRef (C_Tuple);
      Py_DecRef (D_Tuple);
      Py_DecRef (PyParams);
      Py_DecRef (PyResult);

   end Call;

   --  -------------------------------------------------------------------------

end Python_CLF;
