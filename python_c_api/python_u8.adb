
with Interfaces.C;
with Tuple_Builder; use Tuple_Builder;

package body Python_U8 is
   use Python;
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

   function Call (M : Python.Module; Function_Name : String; A : Integer;
                  B : ML_Arrays_And_Matrices.Real_Float_Matrix)
                  return Python_API.PyObject is
      use Interfaces.C;

      function Py_BuildValue (Format  : char_array; A : int;
                              T1      : PyObject) return PyObject;
      pragma Import (C, Py_BuildValue, "Py_BuildValue");
      F        : constant PyObject := Python.Get_Symbol (M, Function_Name);
      A_Tuple  : constant PyObject := To_Tuple (B);
      PyParams : PyObject;
      PyResult : PyObject;
   begin
      PyParams := Py_BuildValue (To_C ("iO"), int (A), A_Tuple);
      PyResult := Call_Object (F, PyParams);

      Py_DecRef (F);
      Py_DecRef (A_Tuple);
      Py_DecRef (PyParams);

      return PyResult;

   end Call;

   -- --------------------------------------------------------------------------

end Python_U8;
