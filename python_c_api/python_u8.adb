
with Interfaces.C;

--  with Ada.Text_IO; use Ada.Text_IO;

with Tuple_Builder;
with U8_Tuple_Builder; use U8_Tuple_Builder;

package body Python_U8 is
   use Python_API;

   --  -------------------------------------------------------------------------

   procedure Call (M : Python.Module; Function_Name : String;
                   A : ML_U8_Types.Unsigned_8_Array_3D) is
      use Interfaces.C;

      function Py_BuildValue (Format  : Interfaces.C.char_array;
                              T1      : PyObject_Ptr;
                              I1      : int) return PyObject_Ptr;
      pragma Import (C, Py_BuildValue, "Py_BuildValue");

--        Routine_Name : constant String := "Python_U8.Call U8_3D ";
      F            : PyObject_Ptr;
      Row_Length   : constant int := int (A'Length (2));
      A_Tuple      : PyObject_Ptr;
      PyParams     : PyObject_Ptr;
      PyResult     : PyObject_Ptr;
   begin
      F := Python.Get_Symbol (M, Function_Name);
      A_Tuple := To_Tuple (A);
      PyParams :=
        Py_BuildValue (Interfaces.C.To_C ("Oi"), A_Tuple, Row_Length);
      Py_DecRef (A_Tuple);

      PyResult := Python.Call_Object (F, PyParams);

      Py_DecRef (F);
      Py_DecRef (PyParams);
      Py_DecRef (PyResult);

   end Call;

   --  -------------------------------------------------------------------------

   procedure Call (M : Python.Module; Function_Name : String;
                   A : Python_API.PyObject_Ptr; B : Image_64_Vector;
                   C : Integer_Array; D : Image_64_Vector;
                   E : Integer_Array) is
      use Tuple_Builder;

      function Py_BuildValue (Format             : Interfaces.C.char_array;
                              O1, T1, T2, T3, T4 : PyObject_Ptr)
                              return PyObject_Ptr;
      pragma Import (C, Py_BuildValue, "Py_BuildValue");

      --        Routine_Name : constant String := "Python_U8.Call ABCD ";
      F            : PyObject_Ptr;
      B_Tuple      : PyObject_Ptr;
      C_Tuple      : PyObject_Ptr;
      D_Tuple      : PyObject_Ptr;
      E_Tuple      : PyObject_Ptr;
      PyParams     : PyObject_Ptr;
      PyResult     : PyObject_Ptr;
   begin
      F := Python.Get_Symbol (M, Function_Name);
      B_Tuple := To_Tuple (B);
      C_Tuple := To_Tuple (C);
      D_Tuple := To_Tuple (D);
      E_Tuple := To_Tuple (E);
      PyParams := Py_BuildValue (Interfaces.C.To_C ("OOOOO"),
                                 A, B_Tuple, C_Tuple, D_Tuple, E_Tuple);
      Py_DecRef (B_Tuple);
      Py_DecRef (C_Tuple);
      Py_DecRef (D_Tuple);
      Py_DecRef (E_Tuple);

      PyResult := Python.Call_Object (F, PyParams);

      Py_DecRef (F);
      Py_DecRef (PyParams);
      Py_DecRef (PyResult);

   end Call;

   --  -------------------------------------------------------------------------

end Python_U8;
