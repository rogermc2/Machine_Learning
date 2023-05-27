
with Interfaces.C;
with Python_API;
with Tuple_Builder; use Tuple_Builder;

package body Python_U8 is
   use Python_API;

   procedure Call (M : Python.Module; Function_Name : String;
                   A : ML_U8_Types.Unsigned_8_Array_3D) is
      use Interfaces.C;

      function Py_BuildValue (Format  : Interfaces.C.char_array;
                              T1      : PyObject;
                              I1      : int) return PyObject;
      pragma Import (C, Py_BuildValue, "Py_BuildValue");

      --        Routine_Name : constant String := "Python.Call U8_3D ";
      F            : PyObject;
      Row_Length   : constant int := int (A'Length (2));
      A_Tuple      : PyObject;
      PyParams     : PyObject;
      PyResult     : PyObject;
   begin
      F := Python.Get_Symbol (M, Function_Name);
      A_Tuple := To_Tuple (A);
      PyParams := Py_BuildValue (Interfaces.C.To_C ("Oi"),
                                 A_Tuple, Row_Length);
      PyResult := Python.Call_Object (F, PyParams);

      Py_DecRef (F);
      Py_DecRef (A_Tuple);
      Py_DecRef (PyParams);
      Py_DecRef (PyResult);

   end Call;

   --  -------------------------------------------------------------------------

end Python_U8;
