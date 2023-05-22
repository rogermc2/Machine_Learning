
with Interfaces.C;

--  with Ada.Assertions; use Ada.Assertions;

with Parsers;
with Python_API; use Python_API;
with Tuple_Builder; use Tuple_Builder;

package body Python_Class is

   procedure Call (M   : Python.Module; Function_Name : String;
                   CLF : PyClass) is
      use Interfaces.C;

      function Py_BuildValue (Format : char_array; O1 : PyClass)
                              return PyObject;
      pragma Import (C, Py_BuildValue, "Py_BuildValue");

      F        : constant PyObject := Python.Get_Symbol (M, Function_Name);
      PyParams : PyObject;
      PyResult : PyObject;
   begin
      PyParams := Py_BuildValue (To_C ("(O)"), CLF);
      PyResult := Python.Call_Object (F, PyParams);

      Py_DecRef (F);
      Py_DecRef (PyParams);
      Py_DecRef (PyResult);

   end Call;

   -- --------------------------------------------------------------------------

   function Call (M   : Python.Module; Function_Name : String;
                  CLF : PyClass) return PyClass is
      use Interfaces.C;

      function Py_BuildValue (Format   : char_array; O1 : PyClass)
                              return PyObject;
      pragma Import (C, Py_BuildValue, "Py_BuildValue");

      F        : constant PyObject := Python.Get_Symbol (M, Function_Name);
      PyParams : PyObject;
      PyResult : PyClass;
   begin
      PyParams := Py_BuildValue (To_C ("(O)"), CLF);
      PyResult := Python.Call_Object (F, PyParams);

      Py_DecRef (F);
      Py_DecRef (PyParams);

      return PyResult;

   end Call;

   -- --------------------------------------------------------------------------

   function Call (M : Python.Module; Function_Name : String; A : Positive)
               return PyClass is
      use Interfaces.C;

      function Py_BuildValue (Format : char_array; A : int) return PyObject;
      pragma Import (C, Py_BuildValue, "Py_BuildValue");

      F        : constant PyObject := Python.Get_Symbol (M, Function_Name);
      PyParams : PyObject;
      PyResult : PyClass;
   begin
      PyParams := Py_BuildValue (To_C ("(i)"), int (A));
      PyResult := Python.Call_Object (F, PyParams);

      Py_DecRef (F);
      Py_DecRef (PyParams);

      return PyResult;

   end Call;

   -- --------------------------------------------------------------------------

   function Call (M : Python.Module; Function_Name : String; CLF : PyClass;
                  A : Float_Vector_List) return Real_Float_Matrix is
      use Interfaces.C;

      --        Routine_Name : constant String := "Python_Class.Call FVL ";

      function Py_BuildValue (Format : char_array; O1 : PyClass; T1 : PyObject)
                           return PyObject;
      pragma Import (C, Py_BuildValue, "Py_BuildValue");

      F            : constant PyObject := Python.Get_Symbol (M, Function_Name);
      A_Tuple      : constant PyObject := To_Tuple (A);
      PyParams     : PyObject;
      Py_Result    : PyObject;
   begin
      --        Assert (CLF /= System.Null_Address, Routine_Name & "CLF is null");
      PyParams := Py_BuildValue (To_C ("OO"), CLF, A_Tuple);
      Py_Result := Python.Call_Object (F, PyParams);

      Py_DecRef (F);
      Py_DecRef (A_Tuple);
      Py_DecRef (PyParams);

      declare
         Result : constant Real_Float_Matrix := Parsers.Parse_Tuple (Py_Result);
      begin
         Py_DecRef (Py_Result);
         return Result;
      end;

   end Call;

   -- --------------------------------------------------------------------------

   function Call (M   : Python.Module; Function_Name : String;
                  CLF : PyClass;
                  A   : Integer_Array) return Integer_Array is
      use Interfaces.C;

      function Py_BuildValue (Format : char_array; O1 : PyClass;
                              T1     : PyObject) return PyObject;
      pragma Import (C, Py_BuildValue, "Py_BuildValue");

      F        : constant PyObject := Python.Get_Symbol (M, Function_Name);
      A_Tuple  : constant PyObject := To_Tuple (A);
      PyParams : PyObject;
      PyResult : PyObject;
   begin
      PyParams := Py_BuildValue (To_C ("OO"), CLF, A_Tuple);
      PyResult := Python.Call_Object (F, PyParams);

      Py_DecRef (F);
      Py_DecRef (A_Tuple);
      Py_DecRef (PyParams);

      declare
         Result : constant Integer_Array := Parsers.Parse_Tuple (PyResult);
      begin
         Py_DecRef (PyResult);

         return Result;
      end;

   end Call;

   -- --------------------------------------------------------------------------

   function Call (M   : Python.Module; Function_Name : String;
                  CLF : PyClass; A : Integer_Matrix)
                  return Real_Float_Vector is
      use Interfaces.C;

      function Py_BuildValue (Format : char_array; O1 : PyClass;
                              T1     : PyObject) return PyObject;
      pragma Import (C, Py_BuildValue, "Py_BuildValue");

      F        : constant PyObject := Python.Get_Symbol (M, Function_Name);
      A_Tuple  : constant PyObject := To_Tuple (A);
      PyParams : PyObject;
      PyResult : PyObject;
   begin
      PyParams := Py_BuildValue (To_C ("OO"), CLF, A_Tuple);
      PyResult := Python.Call_Object (F, PyParams);

      Py_DecRef (F);
      Py_DecRef (A_Tuple);
      Py_DecRef (PyParams);

      declare
         Result : constant Real_Float_Vector := Parsers.Parse_Tuple (PyResult);
      begin
         Py_DecRef (PyResult);

         return Result;
      end;

   end Call;

   -- --------------------------------------------------------------------------

   function Call (M   : Python.Module; Function_Name : String;
                  CLF : PyClass;
                  A   : Integer_Matrix) return Integer_Array is
      use Interfaces.C;

      function Py_BuildValue (Format : char_array; O1 : PyClass;
                              T1     : PyObject) return PyObject;
      pragma Import (C, Py_BuildValue, "Py_BuildValue");

      F        : constant PyObject := Python.Get_Symbol (M, Function_Name);
      A_Tuple  : constant PyObject := To_Tuple (A);
      PyParams : PyObject;
      PyResult : PyObject;
   begin
      PyParams := Py_BuildValue (To_C ("OO"), CLF, A_Tuple);
      PyResult := Python.Call_Object (F, PyParams);

      Py_DecRef (F);
      Py_DecRef (A_Tuple);
      Py_DecRef (PyParams);

      declare
         Result : constant Integer_Array := Parsers.Parse_Tuple (PyResult);
      begin
         Py_DecRef (PyResult);

         return Result;
      end;

   end Call;

   -- --------------------------------------------------------------------------

   function Call (M : Python.Module; Function_Name : String;
                  A : Float_Vector_List; B : ML_Types.Integer_List)
               return PyClass is
      use Interfaces.C;

      function Py_BuildValue (Format : char_array; T1, T2: PyObject)
                           return PyObject;
      pragma Import (C, Py_BuildValue, "Py_BuildValue");

      F        : constant PyObject := Python.Get_Symbol (M, Function_Name);
      A_Tuple  : constant PyObject := To_Tuple (A);
      B_Tuple  : constant PyObject := To_Tuple (B);
      PyParams : PyObject;
      PyResult : PyObject;
   begin
      PyParams := Py_BuildValue (To_C ("OO"), A_Tuple, B_Tuple);
      PyResult := Python.Call_Object (F, PyParams);

      Py_DecRef (F);
      Py_DecRef (A_Tuple);
      Py_DecRef (B_Tuple);
      Py_DecRef (PyParams);

      return PyResult;

   end Call;

   -- --------------------------------------------------------------------------

   function Call (M : Python.Module; Function_Name : String;
                  A : Integer_Array_List; B : ML_Types.Integer_List)
               return PyClass is
      use Interfaces.C;

      function Py_BuildValue (Format : char_array; T1, T2: PyObject)
                           return PyObject;
      pragma Import (C, Py_BuildValue, "Py_BuildValue");

      F        : constant PyObject := Python.Get_Symbol (M, Function_Name);
      A_Tuple  : constant PyObject := To_Tuple (A);
      B_Tuple  : constant PyObject := To_Tuple (B);
      PyParams : PyObject;
      PyResult : PyObject;
   begin
      PyParams := Py_BuildValue (To_C ("OO"), A_Tuple, B_Tuple);
      PyResult := Python.Call_Object (F, PyParams);

      Py_DecRef (F);
      Py_DecRef (A_Tuple);
      Py_DecRef (B_Tuple);
      Py_DecRef (PyParams);

      return PyResult;

   end Call;

   -- --------------------------------------------------------------------------

   function Call (M   : Python.Module; Function_Name : String;
                  CLF : PyClass;
                  A   : Real_Float_Matrix) return Real_Float_Vector is
      use Interfaces.C;

      function Py_BuildValue (Format : char_array; O1 : PyClass;
                              T1     : PyObject) return PyObject;
      pragma Import (C, Py_BuildValue, "Py_BuildValue");

      F        : constant PyObject := Python.Get_Symbol (M, Function_Name);
      A_Tuple  : constant PyObject := To_Tuple (A);
      PyParams : PyObject;
      PyResult : PyObject;
   begin
      PyParams := Py_BuildValue (To_C ("OO"), CLF, A_Tuple);
      PyResult := Python.Call_Object (F, PyParams);

      Py_DecRef (F);
      Py_DecRef (A_Tuple);
      Py_DecRef (PyParams);

      declare
         Result : constant Real_Float_Vector := Parsers.Parse_Tuple (PyResult);
      begin
         Py_DecRef (PyResult);

         return Result;
      end;

   end Call;

   -- --------------------------------------------------------------------------

end Python_Class;
