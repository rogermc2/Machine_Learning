--  Based on inspirel_ada-python_demo

with System; use System;
with Interfaces.C;

with Ada.Assertions; use Ada.Assertions;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Unchecked_Conversion;

--  with Basic_Printing; use Basic_Printing;
with Parsers;
with Tuple_Builder; use Tuple_Builder;

package body Python_CLF is

   --  -------------------------------------------------------------------------

   function Call (M   : Python.Module; Function_Name : String;
                  Obj : PyObject_Ptr) return Integer is
      use Interfaces.C;

      function Py_BuildValue (Format : char_array; O1 : PyObject_Ptr)
                              return PyObject_Ptr;
      pragma Import (C, Py_BuildValue, "Py_BuildValue");

      Routine_Name : constant String := "Python_CLF.Call Integers ";
      F            : constant PyObject_Ptr :=
        Python.Get_Symbol (M, Function_Name);
      PyParams     : PyObject_Ptr;
      PyResult     : PyObject_Ptr;
      Result       : Integer;
   begin
      Assert (Obj /= System.Null_Address, Routine_Name & "Obj is null");
      PyParams := Py_BuildValue (To_C ("(O)"), Obj);
      PyResult := Python.Call_Object (F, PyParams);
      if PyResult = System.Null_Address then
         Put (Routine_Name & "Py error message: ");
         PyErr_Print;
      end if;

      Result := Integer (PyInt_AsLong (PyResult));

      Py_DecRef (F);
      Py_DecRef (PyParams);
      Py_DecRef (PyResult);

      return Result;

   end Call;

   -- --------------------------------------------------------------------------

   function Call (M   : Python.Module; Function_Name : String;
                  Obj : PyObject_Ptr) return ML_Types.Unbounded_List is
      use Interfaces.C;
      use ML_Types;

      function Py_BuildValue (Format : char_array; O1 : PyObject_Ptr)
                              return PyObject_Ptr;
      pragma Import (C, Py_BuildValue, "Py_BuildValue");

      Routine_Name : constant String := "Python_CLF.Call Integers ";
      F            : constant PyObject_Ptr :=
        Python.Get_Symbol (M, Function_Name);
      PyParams     : PyObject_Ptr;
      PyResult     : PyObject_Ptr;
      Result       : Unbounded_List;
   begin
      Assert (Obj /= System.Null_Address, Routine_Name & "Obj is null");
      PyParams := Py_BuildValue (To_C ("(O)"), Obj);
      PyResult := Python.Call_Object (F, PyParams);
      if PyResult = System.Null_Address then
         Put (Routine_Name & "Py error message: ");
         PyErr_Print;
      end if;

      Result := Parsers.Parse_Tuple (PyResult);

      Py_DecRef (F);
      Py_DecRef (PyParams);
      Py_DecRef (PyResult);

      return Result;

   end Call;

   -- --------------------------------------------------------------------------

   function Call (M : Python.Module; Function_Name : String; A : Positive)
                  return PyObject_Ptr is
      use Interfaces.C;

      function Py_BuildValue (Format : char_array; A : int) return PyObject_Ptr;
      pragma Import (C, Py_BuildValue, "Py_BuildValue");

      F        : constant PyObject_Ptr := Python.Get_Symbol (M, Function_Name);
      PyParams : PyObject_Ptr;
      PyResult : PyObject_Ptr;
   begin
      PyParams := Py_BuildValue (To_C ("(i)"), int (A));
      PyResult := Python.Call_Object (F, PyParams);

      Py_DecRef (F);
      Py_DecRef (PyParams);

      return PyResult;

   end Call;

   -- --------------------------------------------------------------------------

   function Call (M : Python.Module; Function_Name : String; A : PyObject_Ptr)
                  return PyObject_Ptr is
      use Interfaces.C;

      function Py_BuildValue (Format : char_array; A : PyObject_Ptr)
                              return PyObject_Ptr;
      pragma Import (C, Py_BuildValue, "Py_BuildValue");

      F        : constant PyObject_Ptr := Python.Get_Symbol (M, Function_Name);
      PyParams : PyObject_Ptr;
      PyResult : PyObject_Ptr;
   begin
      PyParams := Py_BuildValue (To_C ("(O)"), A);
      PyResult := Python.Call_Object (F, PyParams);

      Py_DecRef (F);
      Py_DecRef (PyParams);

      return PyResult;

   end Call;

   -- --------------------------------------------------------------------------

   procedure Call (M   : Python.Module; Function_Name : String;
                   CLF : PyObject_Ptr; A : Float_Array_List;
                   B   : ML_Types.Integer_List) is
      use Interfaces.C;

      function Py_BuildValue (Format : char_array; O1, T1, T2: PyObject_Ptr)
                              return PyObject_Ptr;
      pragma Import (C, Py_BuildValue, "Py_BuildValue");

      F        : constant PyObject_Ptr := Python.Get_Symbol (M, Function_Name);
      A_Tuple  : constant PyObject_Ptr := To_Tuple (A);
      B_Tuple  : constant PyObject_Ptr := To_Tuple (B);
      PyParams : PyObject_Ptr;
      PyResult : PyObject_Ptr;
   begin
      PyParams := Py_BuildValue (To_C ("OOO"), CLF, A_Tuple, B_Tuple);
      PyResult := Python.Call_Object (F, PyParams);

      Py_DecRef (F);
      Py_DecRef (A_Tuple);
      Py_DecRef (B_Tuple);
      Py_DecRef (PyParams);
      Py_DecRef (PyResult);

   end Call;

   -- --------------------------------------------------------------------------

   procedure Call (M   : Python.Module; Function_Name : String;
                   CLF : PyObject_Ptr; A : Float_Vector_List;
                   B   : ML_Types.Integer_List) is
      use Interfaces.C;

      function Py_BuildValue (Format : char_array; O1, T1, T2: PyObject_Ptr)
                              return PyObject_Ptr;
      pragma Import (C, Py_BuildValue, "Py_BuildValue");

      F        : constant PyObject_Ptr := Python.Get_Symbol (M, Function_Name);
      A_Tuple  : constant PyObject_Ptr := To_Tuple (A);
      B_Tuple  : constant PyObject_Ptr := To_Tuple (B);
      PyParams : PyObject_Ptr;
      PyResult : PyObject_Ptr;
   begin
      PyParams := Py_BuildValue (To_C ("OOO"), CLF, A_Tuple, B_Tuple);
      PyResult := Python.Call_Object (F, PyParams);

      Py_DecRef (F);
      Py_DecRef (A_Tuple);
      Py_DecRef (B_Tuple);
      Py_DecRef (PyParams);
      Py_DecRef (PyResult);

   end Call;

   -- --------------------------------------------------------------------------

   function Call (M   : Python.Module; Function_Name : String;
                  Obj : PyObject_Ptr; A : Integer) return Float is
      use Interfaces.C;

      function Py_BuildValue (Format     : char_array;
                              O1         : PyObject_Ptr; I1 : int)
                              return PyObject_Ptr;
      pragma Import (C, Py_BuildValue, "Py_BuildValue");

      Routine_Name : constant String := "Python_CLF.Call Integers ";
      F            : constant PyObject_Ptr :=
        Python.Get_Symbol (M, Function_Name);
      PyParams     : PyObject_Ptr;
      PyResult     : PyObject_Ptr;
      Result       : Float;
   begin
      Assert (Obj /= System.Null_Address, Routine_Name & "Obj is null");
      PyParams := Py_BuildValue (To_C ("Oi"), Obj, int (A));
      PyResult := Python.Call_Object (F, PyParams);
      if PyResult = System.Null_Address then
         Put (Routine_Name & "Py error message: ");
         PyErr_Print;
      end if;

      Result := Float (PyFloat_AsDouble (PyResult));

      Py_DecRef (F);
      Py_DecRef (PyParams);
      Py_DecRef (PyResult);

      return Result;

   end Call;

   -- --------------------------------------------------------------------------

   function Call (M : Python.Module; Function_Name : String; A, B : Integer)
                  return PyObject_Ptr is
      use Interfaces.C;

      function Py_BuildValue (Format : char_array; I1, I2: int)
                              return PyObject_Ptr;
      pragma Import (C, Py_BuildValue, "Py_BuildValue");

      Routine_Name : constant String := "Python_CLF.Call Integers ";
      F            : constant PyObject_Ptr :=
        Python.Get_Symbol (M, Function_Name);
      PyParams     : PyObject_Ptr;
      PyResult     : PyObject_Ptr;
   begin
      PyParams := Py_BuildValue (To_C ("ii"), int (A), int (B));
      PyResult := Python.Call_Object (F, PyParams);

      if PyResult = System.Null_Address then
         Put (Routine_Name & "Py error message: ");
         PyErr_Print;
      end if;

      Py_DecRef (F);
      Py_DecRef (PyParams);

      return PyResult;

   end Call;

   -- --------------------------------------------------------------------------

   function Call (M   : Python.Module; Function_Name : String;
                  Obj : PyObject_Ptr; A, B : Integer) return Float is
      use Interfaces.C;

      function Py_BuildValue (Format : char_array;
                              O1     : PyObject_Ptr; I1, I2: int)
                              return PyObject_Ptr;
      pragma Import (C, Py_BuildValue, "Py_BuildValue");

      Routine_Name : constant String := "Python_CLF.Call Integers ";
      F            : constant PyObject_Ptr :=
        Python.Get_Symbol (M, Function_Name);
      PyParams     : PyObject_Ptr;
      PyResult     : PyObject_Ptr;
      Result       : Float;
   begin
      Assert (Obj /= System.Null_Address, Routine_Name & "Obj is null");
      PyParams := Py_BuildValue (To_C ("Oii"), Obj, int (A), int (B));
      PyResult := Python.Call_Object (F, PyParams);
      if PyResult = System.Null_Address then
         Put (Routine_Name & "Py error message: ");
         PyErr_Print;
      end if;

      Result := Float (PyFloat_AsDouble (PyResult));

      Py_DecRef (F);
      Py_DecRef (PyParams);
      Py_DecRef (PyResult);

      return Result;

   end Call;

   -- --------------------------------------------------------------------------

   function Call (M : Python.Module; Function_Name : String; CLF : PyObject_Ptr;
                  A : Integer_Array_List) return Integer_Matrix is
      use Interfaces.C;
      Routine_Name : constant String := "Python_CLF.Call IL2D ";

      function Py_BuildValue (Format : char_array; O1, T1 : PyObject_Ptr)
                              return PyObject_Ptr;
      pragma Import (C, Py_BuildValue, "Py_BuildValue");

      F            : constant PyObject_Ptr :=
        Python.Get_Symbol (M, Function_Name);
      A_Tuple      : constant PyObject_Ptr := To_Tuple (A);
      PyParams     : PyObject_Ptr;
      Py_Result    : PyObject_Ptr;
   begin
      Assert (CLF /= System.Null_Address, Routine_Name & "CLF is null");
      PyParams := Py_BuildValue (To_C ("OO"), CLF, A_Tuple);
      Py_Result := Python.Call_Object (F, PyParams);

      Py_DecRef (F);
      Py_DecRef (A_Tuple);
      Py_DecRef (PyParams);

      declare
         Result : constant Integer_Matrix := Parsers.Parse_Tuple (Py_Result);
      begin
         Py_DecRef (Py_Result);
         return Result;
      end;

   end Call;

   -- --------------------------------------------------------------------------

   function Call (M : Python.Module; Function_Name : String; CLF : PyObject_Ptr;
                  A : Integer_Array_List) return Real_Float_Matrix is
      use Interfaces.C;
      Routine_Name : constant String := "Python_CLF.Call IAL ";

      function Py_BuildValue (Format : char_array; O1, T1 : PyObject_Ptr)
                              return PyObject_Ptr;
      pragma Import (C, Py_BuildValue, "Py_BuildValue");

      F            : constant PyObject_Ptr :=
        Python.Get_Symbol (M, Function_Name);
      A_Tuple      : constant PyObject_Ptr := To_Tuple (A);
      PyParams     : PyObject_Ptr;
      Py_Result    : PyObject_Ptr;
   begin
      Assert (CLF /= System.Null_Address, Routine_Name & "CLF is null");
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

   function Call (M : Python.Module; Function_Name : String; CLF : PyObject_Ptr;
                  A : Float_Vector_List) return Real_Float_Matrix is
      use Interfaces.C;
      Routine_Name : constant String := "Python_CLF.Call FVL ";

      function Py_BuildValue (Format : char_array; O1, T1 : PyObject_Ptr)
                              return PyObject_Ptr;
      pragma Import (C, Py_BuildValue, "Py_BuildValue");

      F            : constant PyObject_Ptr :=
        Python.Get_Symbol (M, Function_Name);
      A_Tuple      : constant PyObject_Ptr := To_Tuple (A);
      PyParams     : PyObject_Ptr;
      Py_Result    : PyObject_Ptr;
   begin
      Assert (CLF /= System.Null_Address, Routine_Name & "CLF is null");
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

   function Call (M : Python.Module; Function_Name : String; CLF : PyObject_Ptr;
                  A : Real_Float_Matrix) return Real_Float_Matrix is
      use Interfaces.C;
      Routine_Name : constant String := "Python_CLF.Call RFM ";

      function Py_BuildValue (Format : char_array; O1, T1 : PyObject_Ptr)
                              return PyObject_Ptr;
      pragma Import (C, Py_BuildValue, "Py_BuildValue");

      F            : constant PyObject_Ptr :=
        Python.Get_Symbol (M, Function_Name);
      A_Tuple      : constant PyObject_Ptr := To_Tuple (A);
      PyParams     : PyObject_Ptr;
      Py_Result    : PyObject_Ptr;
   begin
      Assert (CLF /= System.Null_Address, Routine_Name & "CLF is null");
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

   procedure Call (M   : Python.Module; Function_Name : String;
                   CLF : PyObject_Ptr; A : ML_Types.Integer_List_2D) is
      use Interfaces.C;
      Routine_Name : constant String := "Python_CLF.Call IL2D ";

      function Py_BuildValue (Format : char_array; O1, T1 : PyObject_Ptr)
                              return PyObject_Ptr;
      pragma Import (C, Py_BuildValue, "Py_BuildValue");

      F            : constant PyObject_Ptr :=
        Python.Get_Symbol (M, Function_Name);
      A_Tuple      : constant PyObject_Ptr := To_Tuple (A);
      PyParams     : PyObject_Ptr;
      PyResult     : PyObject_Ptr;
   begin
      Assert (CLF /= System.Null_Address, Routine_Name & "CLF is null");
      Assert (A_Tuple /= System.Null_Address, Routine_Name &
                "A_Tuple is null");
      PyParams := Py_BuildValue (To_C ("OO"), CLF, A_Tuple);
      PyResult := Python.Call_Object (F, PyParams);

      Py_DecRef (F);
      Py_DecRef (A_Tuple);
      Py_DecRef (PyParams);
      Py_DecRef (PyResult);

   end Call;

   -- -------------------------------------------------------------------------

   procedure Call (M    : Python.Module; Function_Name : String;
                   CLF  : PyObject_Ptr; A, B : ML_Types.Integer_List_2D) is
      use Interfaces.C;
      Routine_Name : constant String := "Python_CLF.Call IL2D2 ";

      function Py_BuildValue (Format : char_array; O1, T1, T2 : PyObject_Ptr)
                              return PyObject_Ptr;
      pragma Import (C, Py_BuildValue, "Py_BuildValue");

      F            : constant PyObject_Ptr :=
        Python.Get_Symbol (M, Function_Name);
      A_Tuple      : constant PyObject_Ptr := To_Tuple (A);
      B_Tuple      : constant PyObject_Ptr := To_Tuple (B);
      PyParams     : PyObject_Ptr;
      PyResult     : PyObject_Ptr;
   begin
      Assert (CLF /= System.Null_Address, Routine_Name & "CLF is null");
      Assert (A_Tuple /= System.Null_Address, Routine_Name &
                "A_Tuple is null");
      Assert (B_Tuple /= System.Null_Address, Routine_Name &
                "B_Tuple is null");
      PyParams := Py_BuildValue (To_C ("OOO"), CLF, A_Tuple, B_Tuple);
      Assert (PyParams /= System.Null_Address, Routine_Name &
                "PyParams is null");
      PyResult := Python.Call_Object (F, PyParams);

      Py_DecRef (F);
      Py_DecRef (A_Tuple);
      Py_DecRef (B_Tuple);
      Py_DecRef (PyParams);
      Py_DecRef (PyResult);
      Put_Line (Routine_Name & "done");

   end Call;

   -- --------------------------------------------------------------------------

   function Call (M : Python.Module; Function_Name : String; CLF : PyObject_Ptr;
                  A : Float_Array_List; B : ML_Types.Integer_List)
                  return Python_API.PyObject_Ptr is
      use Interfaces.C;
      Routine_Name : constant String := "Python_CLF.Call IL2DIL ";

      function Py_BuildValue (Format : char_array; O1, T1, T2 : PyObject_Ptr)
                              return PyObject_Ptr;
      pragma Import (C, Py_BuildValue, "Py_BuildValue");

      F            : constant PyObject_Ptr :=
        Python.Get_Symbol (M, Function_Name);
      A_Tuple      : constant PyObject_Ptr := To_Tuple (A);
      B_Tuple      : constant PyObject_Ptr := To_Tuple (B);
      PyParams     : PyObject_Ptr;
      PyResult     : PyObject_Ptr;
   begin
      Assert (CLF /= System.Null_Address, Routine_Name & "CLF is null");
      Assert (A_Tuple /= System.Null_Address, Routine_Name &
                "A_Tuple is null");
      Assert (B_Tuple /= System.Null_Address, Routine_Name &
                "B_Tuple is null");
      PyParams := Py_BuildValue (To_C ("OOO"), CLF, A_Tuple, B_Tuple);
      Assert (PyParams /= System.Null_Address, Routine_Name &
                "PyParams is null");

      PyResult := Python.Call_Object (F, PyParams);
      Assert (PyResult /= System.Null_Address, Routine_Name &
                "PyResult is null");

      Py_DecRef (F);
      Py_DecRef (A_Tuple);
      Py_DecRef (B_Tuple);
      Py_DecRef (PyParams);

      return PyResult;

   end Call;

   -- --------------------------------------------------------------------------

   function Call (M : Python.Module; Function_Name : String;
                  A : Float_Matrix_List; B : ML_Types.Integer_List)
                  return Python_API.PyObject_Ptr is
      use Interfaces.C;
      Routine_Name : constant String := "Python_CLF.Call IL2DIL ";

      function Py_BuildValue (Format : char_array; T1, T2 : PyObject_Ptr)
                              return PyObject_Ptr;
      pragma Import (C, Py_BuildValue, "Py_BuildValue");

      F            : constant PyObject_Ptr :=
        Python.Get_Symbol (M, Function_Name);
      A_Tuple      : constant PyObject_Ptr := To_Tuple (A);
      B_Tuple      : constant PyObject_Ptr := To_Tuple (B);
      PyParams     : PyObject_Ptr;
      PyResult     : PyObject_Ptr;
   begin
      Assert (A_Tuple /= System.Null_Address, Routine_Name &
                "A_Tuple is null");
      Assert (B_Tuple /= System.Null_Address, Routine_Name &
                "B_Tuple is null");
      PyParams := Py_BuildValue (To_C ("OO"), A_Tuple, B_Tuple);
      Assert (PyParams /= System.Null_Address, Routine_Name &
                "PyParams is null");

      PyResult := Python.Call_Object (F, PyParams);
      Assert (PyResult /= System.Null_Address, Routine_Name &
                "PyResult is null");

      Py_DecRef (F);
      Py_DecRef (A_Tuple);
      Py_DecRef (B_Tuple);
      Py_DecRef (PyParams);

      return PyResult;

   end Call;

   -- --------------------------------------------------------------------------

   procedure Call (M    : Python.Module; Function_Name : String;
                   CLF  : PyObject_Ptr; A    : ML_Types.Integer_List_2D;
                   B    : ML_Types.Integer_List) is
      use Interfaces.C;
      Routine_Name : constant String := "Python_CLF.Call IL2DIL ";

      function Py_BuildValue (Format : char_array; O1, T1, T2 : PyObject_Ptr)
                              return PyObject_Ptr;
      pragma Import (C, Py_BuildValue, "Py_BuildValue");

      F            : constant PyObject_Ptr :=
        Python.Get_Symbol (M, Function_Name);
      A_Tuple      : constant PyObject_Ptr := To_Tuple (A);
      B_Tuple      : constant PyObject_Ptr := To_Tuple (B);
      PyParams     : PyObject_Ptr;
      PyResult     : PyObject_Ptr;
   begin
      Assert (CLF /= System.Null_Address, Routine_Name & "CLF is null");
      Assert (A_Tuple /= System.Null_Address, Routine_Name &
                "A_Tuple is null");
      Assert (B_Tuple /= System.Null_Address, Routine_Name &
                "B_Tuple is null");
      PyParams := Py_BuildValue (To_C ("OOO"), CLF, A_Tuple, B_Tuple);
      Assert (PyParams /= System.Null_Address, Routine_Name &
                "PyParams is null");

      PyResult := Python.Call_Object (F, PyParams);
      Assert (PyResult /= System.Null_Address, Routine_Name &
                "PyResult is null");

      Py_DecRef (F);
      Py_DecRef (A_Tuple);
      Py_DecRef (B_Tuple);
      Py_DecRef (PyParams);
      Py_DecRef (PyResult);

   end Call;

   -- --------------------------------------------------------------------------

   function Call (M : Python.Module; Function_Name : String; CLF : PyObject_Ptr;
                  A : ML_Types.Integer_List_2D) return Integer_Matrix is
      use Interfaces.C;
      Routine_Name : constant String := "Python_CLF.Call IL2D ";

      function Py_BuildValue (Format : char_array; O1, T1 : PyObject_Ptr)
                              return PyObject_Ptr;
      pragma Import (C, Py_BuildValue, "Py_BuildValue");

      F            : constant PyObject_Ptr :=
        Python.Get_Symbol (M, Function_Name);
      A_Tuple      : constant PyObject_Ptr := To_Tuple (A);
      PyParams     : PyObject_Ptr;
      Py_Result    : PyObject_Ptr;
   begin
      Assert (CLF /= System.Null_Address, Routine_Name & "CLF is null");
      PyParams := Py_BuildValue (To_C ("OO"), CLF, A_Tuple);
      Py_Result := Python.Call_Object (F, PyParams);

      Py_DecRef (F);
      Py_DecRef (A_Tuple);
      Py_DecRef (PyParams);

      declare
         Result : constant Integer_Matrix := Parsers.Parse_Tuple (Py_Result);
      begin
         Py_DecRef (Py_Result);
         return Result;
      end;

   end Call;

   -- --------------------------------------------------------------------------

   procedure Call (M   : Python.Module; Function_Name : String;
                   CLF : PyObject_Ptr) is
      use Python;

      function Py_BuildValue (Format : Interfaces.C.char_array;
                              C1     : PyObject_Ptr)  return PyObject_Ptr;
      pragma Import (C, Py_BuildValue, "Py_BuildValue");

      Routine_Name : constant String := "Python_CLF.Call ";
      PyFunc       : constant PyObject_Ptr := Get_Symbol (M, Function_Name);
      PyParams     : PyObject_Ptr;
      Py_Result    : PyObject_Ptr;
   begin
      Assert (CLF /= System.Null_Address, Routine_Name & "CLF is null");
      PyParams :=
        Py_BuildValue (Interfaces.C.To_C ("(O)"), CLF);
      Assert (PyParams /= Null_Address, Routine_Name & "PyParams is null");

      Py_Result := Call_Object (PyFunc, PyParams);
      if Py_Result = System.Null_Address then
         Put (Routine_Name & "Py error message: ");
         PyErr_Print;
      end if;

      Py_DecRef (PyFunc);
      Py_DecRef (PyParams);
      Py_DecRef (Py_Result);

   end Call;

   --  -------------------------------------------------------------------------

   function Call (M   : Python.Module; Function_Name : String;
                  CLF : PyObject_Ptr) return Integer_Array is
      use Interfaces.C;
      use Python;

      function Py_BuildValue (Format : Interfaces.C.char_array;
                              T1     : PyObject_Ptr)  return PyObject_Ptr;
      pragma Import (C, Py_BuildValue, "Py_BuildValue");

      Routine_Name : constant String := "Python_CLF.Call Integer_Array ";
      PyFunc       : constant PyObject_Ptr := Get_Symbol (M, Function_Name);
      PyParams     : PyObject_Ptr;
      Py_Result    : PyObject_Ptr;
   begin
      Assert (CLF /= System.Null_Address, Routine_Name & "CLF is null");
      PyParams := Py_BuildValue (Interfaces.C.To_C ("(O)"), CLF);
      Assert (PyParams /= Null_Address, Routine_Name & "PyParams is null");

      Py_Result := Call_Object (PyFunc, PyParams);
      if Py_Result = System.Null_Address then
         Put (Routine_Name & "Py error message: ");
         PyErr_Print;
      end if;

      Py_DecRef (PyFunc);
      Py_DecRef (PyParams);

      declare
         Result     : Integer_Array (1 .. Integer (PyTuple_Size (Py_Result)));
      begin
         Parsers.Parse_Tuple (Py_Result, Result);
         Py_DecRef (Py_Result);
         return Result;
      end;

   end Call;

   --  -------------------------------------------------------------------------

   function Call (M   : Python.Module; Function_Name : String;
                  CLF : PyObject_Ptr) return ML_Types.Integer_List is
      use Python;

      function Py_BuildValue (Format : Interfaces.C.char_array;
                              T1     : PyObject_Ptr)  return PyObject_Ptr;
      pragma Import (C, Py_BuildValue, "Py_BuildValue");

      Routine_Name : constant String := "Python_CLF.Call Integer_Array ";
      PyFunc       : constant PyObject_Ptr := Get_Symbol (M, Function_Name);
      PyParams     : PyObject_Ptr;
      Py_Result    : PyObject_Ptr;
      Result       : ML_Types.Integer_List;
   begin
      Assert (CLF /= System.Null_Address, Routine_Name & "CLF is null");
      PyParams := Py_BuildValue (Interfaces.C.To_C ("(O)"), CLF);
      Assert (PyParams /= Null_Address, Routine_Name & "PyParams is null");

      Py_Result := Call_Object (PyFunc, PyParams);
      if Py_Result = System.Null_Address then
         Put (Routine_Name & "Py error message: ");
         PyErr_Print;
      end if;

      Py_DecRef (PyFunc);
      Py_DecRef (PyParams);

      Result := Parsers.Parse_Tuple (Py_Result);
      Py_DecRef (Py_Result);

      return Result;

   end Call;

   --  -------------------------------------------------------------------------

   function Call (M   : Python.Module; Function_Name : String;
                  CLF : PyObject_Ptr; A : Integer_Array_List)
                  return Integer_Array is
      use Interfaces.C;
      use Python;

      function Py_BuildValue (Format : Interfaces.C.char_array;
                              T1, T2 : PyObject_Ptr)  return PyObject_Ptr;
      pragma Import (C, Py_BuildValue, "Py_BuildValue");

      Routine_Name : constant String := "Python_CLF.Call Integer_Array_List ";
      PyFunc       : constant PyObject_Ptr := Get_Symbol (M, Function_Name);
      A_Tuple      : constant PyObject_Ptr := To_Tuple (A);
      PyParams     : PyObject_Ptr;
      Py_Result    : PyObject_Ptr;
   begin
      Assert (CLF /= System.Null_Address, Routine_Name & "CLF is null");
      Assert (A_Tuple /= Null_Address, Routine_Name & "A_Tuple is null");

      PyParams :=
        Py_BuildValue (Interfaces.C.To_C ("OO"), CLF, A_Tuple);
      Assert (PyParams /= Null_Address, Routine_Name & "PyParams is null");

      Py_Result := Call_Object (PyFunc, PyParams);
      if Py_Result = System.Null_Address then
         Put (Routine_Name & "Py error message: ");
         PyErr_Print;
      end if;

      Py_DecRef (PyFunc);
      Py_DecRef (A_Tuple);
      Py_DecRef (PyParams);

      declare
         Result     : Integer_Array (1 .. Integer (PyTuple_Size (Py_Result)));
      begin
         Parsers.Parse_Tuple (Py_Result, Result);
         Py_DecRef (Py_Result);
         return Result;
      end;

   end Call;

   --  -------------------------------------------------------------------------

   function Call (M   : Python.Module; Function_Name : String;
                  CLF : PyObject_Ptr; A : ML_Types.Integer_List)
                  return Integer_Array is
      use Interfaces.C;
      use Python;

      function Py_BuildValue (Format : Interfaces.C.char_array;
                              T1, T2 : PyObject_Ptr)  return PyObject_Ptr;
      pragma Import (C, Py_BuildValue, "Py_BuildValue");

      Routine_Name : constant String := "Python_CLF.Call Integer_Array_List ";
      PyFunc       : constant PyObject_Ptr := Get_Symbol (M, Function_Name);
      A_Tuple      : constant PyObject_Ptr := To_Tuple (A);
      PyParams     : PyObject_Ptr;
      Py_Result    : PyObject_Ptr;
   begin
      Assert (CLF /= System.Null_Address, Routine_Name & "CLF is null");
      Assert (A_Tuple /= Null_Address, Routine_Name & "A_Tuple is null");

      PyParams :=
        Py_BuildValue (Interfaces.C.To_C ("OO"), CLF, A_Tuple);
      Assert (PyParams /= Null_Address, Routine_Name & "PyParams is null");

      Py_Result := Call_Object (PyFunc, PyParams);
      if Py_Result = System.Null_Address then
         Put (Routine_Name & "Py error message: ");
         PyErr_Print;
      end if;

      Py_DecRef (PyFunc);
      Py_DecRef (A_Tuple);
      Py_DecRef (PyParams);

      declare
         Result : Integer_Array (1 .. Integer (PyTuple_Size (Py_Result)));
      begin
         Parsers.Parse_Tuple (Py_Result, Result);
         Py_DecRef (Py_Result);
         return Result;
      end;

   end Call;

   --  -------------------------------------------------------------------------

   procedure Call (M   : Python.Module; Function_Name : String;
                   CLF : in out PyObject_Ptr;
                   A   : Integer_Array_List; B : ML_Types.Integer_List) is
      use Python;

      function Py_BuildValue (Format     : Interfaces.C.char_array;
                              T1, T2, T3 : PyObject_Ptr)  return PyObject_Ptr;
      pragma Import (C, Py_BuildValue, "Py_BuildValue");

      Routine_Name : constant String :=
        "Python_CLF.Call 4 * Integer_Array_List ";
      PyFunc       : constant PyObject_Ptr := Get_Symbol (M, Function_Name);
      A_Tuple      : constant PyObject_Ptr := To_Tuple (A);
      B_Tuple      : constant PyObject_Ptr := To_Tuple (B);
      PyParams     : PyObject_Ptr;
   begin
      Assert (CLF /= System.Null_Address, Routine_Name & "CLF is null");
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
                   CLF : in out PyObject_Ptr; A, B : ML_Types.Integer_List) is
      use Python;

      function Py_BuildValue (Format     : Interfaces.C.char_array;
                              T1, T2, T3 : PyObject_Ptr)  return PyObject_Ptr;
      pragma Import (C, Py_BuildValue, "Py_BuildValue");

      Routine_Name : constant String :=
        "Python_CLF.Call 4 * Integer_Array_List ";
      PyFunc       : constant PyObject_Ptr := Get_Symbol (M, Function_Name);
      A_Tuple      : constant PyObject_Ptr := To_Tuple (A);
      B_Tuple      : constant PyObject_Ptr := To_Tuple (B);
      PyParams     : PyObject_Ptr;
   begin
      Assert (CLF /= System.Null_Address, Routine_Name & "CLF is null");
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
                   CLF : PyObject_Ptr; A : Real_Float_List) is
      use Python;

      function Py_BuildValue (Format : Interfaces.C.char_array;
                              O1, T1 : PyObject_Ptr)  return PyObject_Ptr;
      pragma Import (C, Py_BuildValue, "Py_BuildValue");

      Routine_Name : constant String := "Python_CLF.Call RFL ";
      PyFunc       : constant PyObject_Ptr := Get_Symbol (M, Function_Name);
      A_Tuple      : constant PyObject_Ptr := To_Tuple (A);
      PyParams     : PyObject_Ptr;
      PyResult     : PyObject_Ptr;
   begin
      Assert (CLF /= Null_Address, Routine_Name & "CLF is null");
      Assert (PyFunc /= Null_Address, Routine_Name & "PyFunc is null");
      Assert (A_Tuple /= Null_Address, Routine_Name & "A_Tuple is null");

      PyParams :=
        Py_BuildValue (Interfaces.C.To_C ("OO"), CLF, A_Tuple);
      Assert (PyParams /= Null_Address, Routine_Name & "PyParams is null");

      PyResult := Call_Object (PyFunc, PyParams);

      if PyResult = System.Null_Address then
         Put (Routine_Name & "Py error message: ");
         PyErr_Print;
      end if;

      Py_DecRef (PyFunc);
      Py_DecRef (A_Tuple);
      Py_DecRef (PyParams);
      Py_DecRef (PyResult);

   end Call;

   --  -------------------------------------------------------------------------

   procedure Call (M   : Python.Module; Function_Name : String;
                   CLF : PyObject_Ptr; A : Real_Float_Matrix) is
      use Python;

      function Py_BuildValue (Format : Interfaces.C.char_array;
                              O1, T1 : PyObject_Ptr)  return PyObject_Ptr;
      pragma Import (C, Py_BuildValue, "Py_BuildValue");

      Routine_Name : constant String := "Python_CLF.Call RFM IM ";
      PyFunc       : constant PyObject_Ptr := Get_Symbol (M, Function_Name);
      A_Tuple      : constant PyObject_Ptr := To_Tuple (A);
      PyParams     : PyObject_Ptr;
      PyResult     : PyObject_Ptr;
   begin
      Assert (CLF /= Null_Address, Routine_Name & "CLF is null");
      Assert (PyFunc /= Null_Address, Routine_Name & "PyFunc is null");
      Assert (A_Tuple /= Null_Address, Routine_Name & "A_Tuple is null");

      PyParams :=
        Py_BuildValue (Interfaces.C.To_C ("OO"), CLF, A_Tuple);
      Assert (PyParams /= Null_Address, Routine_Name & "PyParams is null");

      PyResult := Call_Object (PyFunc, PyParams);

      if PyResult = System.Null_Address then
         Put (Routine_Name & "Py error message: ");
         PyErr_Print;
      end if;

      Py_DecRef (PyFunc);
      Py_DecRef (A_Tuple);
      Py_DecRef (PyParams);
      Py_DecRef (PyResult);

   end Call;

   --  -------------------------------------------------------------------------

   procedure Call (M   : Python.Module; Function_Name : String;
                   CLF : PyObject_Ptr; A : Real_Float_Matrix;
                   B   : Boolean_Array) is
      use Python;

      function Py_BuildValue (Format     : Interfaces.C.char_array;
                              O1, T1, T2 : PyObject_Ptr)  return PyObject_Ptr;
      pragma Import (C, Py_BuildValue, "Py_BuildValue");

      Routine_Name : constant String := "Python_CLF.Call RFM IM ";
      PyFunc       : constant PyObject_Ptr := Get_Symbol (M, Function_Name);
      A_Tuple      : constant PyObject_Ptr := To_Tuple (A);
      B_Tuple      : constant PyObject_Ptr := To_Tuple (B);
      PyParams     : PyObject_Ptr;
      PyResult     : PyObject_Ptr;
   begin
      Assert (CLF /= Null_Address, Routine_Name & "CLF is null");
      Assert (PyFunc /= Null_Address, Routine_Name & "PyFunc is null");
      Assert (A_Tuple /= Null_Address, Routine_Name & "A_Tuple is null");
      Assert (B_Tuple /= Null_Address, Routine_Name & "B_Tuple is null");

      PyParams :=
        Py_BuildValue (Interfaces.C.To_C ("OOO"), CLF, A_Tuple, B_Tuple);
      Assert (PyParams /= Null_Address, Routine_Name & "PyParams is null");

      PyResult := Call_Object (PyFunc, PyParams);

      if PyResult = System.Null_Address then
         Put (Routine_Name & "Py error message: ");
         PyErr_Print;
      end if;

      Py_DecRef (PyFunc);
      Py_DecRef (A_Tuple);
      Py_DecRef (B_Tuple);
      Py_DecRef (PyParams);
      Py_DecRef (PyResult);

   end Call;

   --  -------------------------------------------------------------------------

   function Call (M : Python.Module; Function_Name : String; CLF : PyObject_Ptr;
                  A : Real_Float_Matrix) return Boolean_Array is
      Routine_Name : constant String := "Python_Clf.Call FM ";

      function Py_BuildValue (Format : Interfaces.C.char_array;
                              O1, T1 : PyObject_Ptr) return PyObject_Ptr;
      pragma Import (C, Py_BuildValue, "Py_BuildValue");

      F        : constant PyObject_Ptr := Python.Get_Symbol (M, Function_Name);
      A_Tuple  : constant PyObject_Ptr := To_Tuple (A);
      PyParams : PyObject_Ptr;
      PyResult : PyObject_Ptr;
      Result   : ML_Arrays_And_Matrices.Boolean_Array (A'Range);
   begin
      Assert (CLF /= System.Null_Address, Routine_Name & "CLF is null");
      PyParams :=
        Py_BuildValue (Interfaces.C.To_C ("OO"), CLF, A_Tuple);

      PyResult := Python.Call_Object (F, PyParams);
      Parsers.Parse_Tuple (PyResult, Result);

      Py_DecRef (F);
      Py_DecRef (A_Tuple);
      Py_DecRef (PyParams);
      Py_DecRef (PyResult);

      return Result;

   end Call;

   --  -------------------------------------------------------------------------

   procedure Call
     (M   : Python.Module; Function_Name : String;
      CLF : PyObject_Ptr; A : Real_Float_Matrix; B : Integer_Matrix) is
      use Python;

      function Py_BuildValue (Format     : Interfaces.C.char_array;
                              O1, T1, T2 : PyObject_Ptr)  return PyObject_Ptr;
      pragma Import (C, Py_BuildValue, "Py_BuildValue");

      Routine_Name : constant String := "Python_CLF.Call RFM IM ";
      PyFunc       : constant PyObject_Ptr := Get_Symbol (M, Function_Name);
      A_Tuple      : constant PyObject_Ptr := To_Tuple (A);
      B_Tuple      : constant PyObject_Ptr := To_Tuple (B);
      PyParams     : PyObject_Ptr;
      PyResult     : PyObject_Ptr;
   begin
      Assert (CLF /= Null_Address, Routine_Name & "CLF is null");
      Assert (PyFunc /= Null_Address, Routine_Name & "PyFunc is null");
      Assert (A_Tuple /= Null_Address, Routine_Name & "A_Tuple is null");
      Assert (B_Tuple /= Null_Address, Routine_Name & "B_Tuple is null");

      PyParams :=
        Py_BuildValue (Interfaces.C.To_C ("OOO"), CLF, A_Tuple, B_Tuple);
      Assert (PyParams /= Null_Address, Routine_Name & "PyParams is null");

      PyResult := Call_Object (PyFunc, PyParams);

      if PyResult = System.Null_Address then
         Put (Routine_Name & "Py error message: ");
         PyErr_Print;
      end if;

      Py_DecRef (PyFunc);
      Py_DecRef (A_Tuple);
      Py_DecRef (B_Tuple);
      Py_DecRef (PyParams);
      Py_DecRef (PyResult);

   end Call;

   --  -------------------------------------------------------------------------

   function Call (M   : Python.Module; Function_Name : String;
                  CLF : PyObject_Ptr; A : Real_Float_Matrix;
                  B   : Integer_Matrix) return Float is
      Routine_Name : constant String := "Python_Clf.Call FIM float ";

      function Py_BuildValue (Format     : Interfaces.C.char_array;
                              O1, T1, T2 : PyObject_Ptr) return PyObject_Ptr;
      pragma Import (C, Py_BuildValue, "Py_BuildValue");

      F        : constant PyObject_Ptr := Python.Get_Symbol (M, Function_Name);
      A_Tuple  : constant PyObject_Ptr := To_Tuple (A);
      B_Tuple  : constant PyObject_Ptr := To_Tuple (B);
      PyParams : PyObject_Ptr;
      PyResult : PyObject_Ptr;
      Result   : Float;
   begin
      Assert (CLF /= System.Null_Address, Routine_Name & "CLF is null");
      PyParams :=
        Py_BuildValue (Interfaces.C.To_C ("OOO"), CLF, A_Tuple, B_Tuple);

      PyResult := Python.Call_Object (F, PyParams);

      if PyResult = System.Null_Address then
         Put (Routine_Name & "Py error message: ");
         PyErr_Print;
      end if;

      Result := Float (PyFloat_AsDouble (PyResult));

      Py_DecRef (F);
      Py_DecRef (A_Tuple);
      Py_DecRef (B_Tuple);
      Py_DecRef (PyParams);
      Py_DecRef (PyResult);

      return Result;

   end Call;

   --  -------------------------------------------------------------------------

   procedure Call
     (M   : Python.Module; Function_Name : String;
      CLF : PyObject_Ptr; A : Real_Float_Matrix; B : Real_Float_Vector) is
      use Python;

      function Py_BuildValue (Format     : Interfaces.C.char_array;
                              O1, T1, T2 : PyObject_Ptr)  return PyObject_Ptr;
      pragma Import (C, Py_BuildValue, "Py_BuildValue");

      Routine_Name : constant String :=
        "Python_CLF.Call 2 * Real_Float_Vector ";
      PyFunc       : constant PyObject_Ptr := Get_Symbol (M, Function_Name);
      A_Tuple      : constant PyObject_Ptr := To_Tuple (A);
      B_Tuple      : constant PyObject_Ptr := To_Tuple (B);
      PyParams     : PyObject_Ptr;
      PyResult     : PyObject_Ptr;
   begin
      Assert (CLF /= Null_Address, Routine_Name & "CLF is null");
      Assert (PyFunc /= Null_Address, Routine_Name & "PyFunc is null");
      Assert (A_Tuple /= Null_Address, Routine_Name & "A_Tuple is null");
      Assert (B_Tuple /= Null_Address, Routine_Name & "B_Tuple is null");

      PyParams :=
        Py_BuildValue (Interfaces.C.To_C ("OOO"), CLF, A_Tuple, B_Tuple);
      Assert (PyParams /= Null_Address, Routine_Name & "PyParams is null");

      PyResult := Call_Object (PyFunc, PyParams);

      if PyResult = System.Null_Address then
         Put (Routine_Name & "Py error message: ");
         PyErr_Print;
      end if;

      Py_DecRef (PyFunc);
      Py_DecRef (A_Tuple);
      Py_DecRef (B_Tuple);
      Py_DecRef (PyParams);
      Py_DecRef (PyResult);

   end Call;

   --  -------------------------------------------------------------------------

   function Call (M   : Python.Module; Function_Name : String;
                  CLF : PyObject_Ptr; A : Real_Float_List)
                  return Real_Float_Vector is
      use Python;
      Routine_Name : constant String :=
        "Python_CLF.Call Real_Float_List ";

      function Py_BuildValue (Format : Interfaces.C.char_array;
                              O1, T1 : PyObject_Ptr)  return PyObject_Ptr;
      pragma Import (C, Py_BuildValue, "Py_BuildValue");

      PyFunc       : constant PyObject_Ptr := Get_Symbol (M, Function_Name);
      A_Tuple      : constant PyObject_Ptr := To_Tuple (A);
      PyParams     : PyObject_Ptr;
      PyResult     : PyObject_Ptr;
      Result       : Real_Float_Vector (1 .. Integer (A.Length));
   begin
      Assert (CLF /= Null_Address, Routine_Name & "CLF is null");
      Assert (PyFunc /= Null_Address, Routine_Name & "PyFunc is null");
      Assert (A_Tuple /= Null_Address, Routine_Name & "A_Tuple is null");

      PyParams :=
        Py_BuildValue (Interfaces.C.To_C ("OO"), CLF, A_Tuple);
      Assert (PyParams /= Null_Address, Routine_Name & "PyParams is null");

      PyResult := Call_Object (PyFunc, PyParams);

      if PyResult = System.Null_Address then
         Put (Routine_Name & "Py error message: ");
         PyErr_Print;
      end if;

      Result := Parsers.Parse_Tuple (PyResult);

      Py_DecRef (PyFunc);
      Py_DecRef (A_Tuple);
      Py_DecRef (PyParams);
      Py_DecRef (PyResult);

      return Result;

   end Call;

   --  -------------------------------------------------------------------------

   function Call (M   : Python.Module; Function_Name : String;
                  CLF : PyObject_Ptr; A : Float_Array)
                  return Real_Float_Matrix is
      use Python;
      Routine_Name : constant String :=
        "Python_CLF.Call Float_Array ";

      function Py_BuildValue (Format : Interfaces.C.char_array;
                              O1, T1 : PyObject_Ptr)  return PyObject_Ptr;
      pragma Import (C, Py_BuildValue, "Py_BuildValue");

      PyFunc       : constant PyObject_Ptr := Get_Symbol (M, Function_Name);
      A_Tuple      : constant PyObject_Ptr := To_Tuple (A);
      PyParams     : PyObject_Ptr;
      PyResult     : PyObject_Ptr;
   begin
      Assert (CLF /= Null_Address, Routine_Name & "CLF is null");
      Assert (PyFunc /= Null_Address, Routine_Name & "PyFunc is null");
      Assert (A_Tuple /= Null_Address, Routine_Name & "A_Tuple is null");

      PyParams :=
        Py_BuildValue (Interfaces.C.To_C ("OO"), CLF, A_Tuple);
      Assert (PyParams /= Null_Address, Routine_Name & "PyParams is null");

      PyResult := Call_Object (PyFunc, PyParams);

      Py_DecRef (PyFunc);
      Py_DecRef (A_Tuple);
      Py_DecRef (PyParams);

      if PyResult = System.Null_Address then
         Put (Routine_Name & "Py error message: ");
         PyErr_Print;
      end if;

      declare
         Result : constant Real_Float_Matrix :=
           Parsers.Parse_Tuple (PyResult);
      begin
         Py_DecRef (PyResult);
         return Result;
      end;

   end Call;

   --  -------------------------------------------------------------------------

   function Call (M   : Python.Module; Function_Name : String;
                  CLF : PyObject_Ptr; A : Real_Float_Matrix)
                  return Real_Float_Vector is
      use Python;
      Routine_Name : constant String :=
        "Python_CLF.Call 2 * Real_Float_Vector ";

      function Py_BuildValue (Format : Interfaces.C.char_array;
                              O1, T1 : PyObject_Ptr)  return PyObject_Ptr;
      pragma Import (C, Py_BuildValue, "Py_BuildValue");

      PyFunc       : constant PyObject_Ptr := Get_Symbol (M, Function_Name);
      A_Tuple      : constant PyObject_Ptr := To_Tuple (A);
      PyParams     : PyObject_Ptr;
      PyResult     : PyObject_Ptr;
      Result       : Real_Float_Vector (A'Range);
   begin
      Assert (CLF /= Null_Address, Routine_Name & "CLF is null");
      Assert (PyFunc /= Null_Address, Routine_Name & "PyFunc is null");
      Assert (A_Tuple /= Null_Address, Routine_Name & "A_Tuple is null");

      PyParams :=
        Py_BuildValue (Interfaces.C.To_C ("OO"), CLF, A_Tuple);
      Assert (PyParams /= Null_Address, Routine_Name & "PyParams is null");

      PyResult := Call_Object (PyFunc, PyParams);

      if PyResult = System.Null_Address then
         Put (Routine_Name & "Py error message: ");
         PyErr_Print;
      end if;

      Py_DecRef (PyFunc);
      Py_DecRef (A_Tuple);
      Py_DecRef (PyParams);

      Result := Parsers.Parse_Tuple (PyResult);
      Py_DecRef (PyResult);

      return Result;

   end Call;

   --  -------------------------------------------------------------------------

   procedure Call (M   : Python.Module; Function_Name : String;
                   CLF : in out PyObject_Ptr;
                   A   : ML_Types.Indef_String_List) is
      use Python;

      function Py_BuildValue (Format  : Interfaces.C.char_array;
                              T1, T2  : PyObject_Ptr)  return PyObject_Ptr;
      pragma Import (C, Py_BuildValue, "Py_BuildValue");

      Routine_Name : constant String := "Python_CLF.Call Unbounded_List ";
      PyFunc       : constant PyObject_Ptr := Get_Symbol (M, Function_Name);
      A_Tuple      : constant PyObject_Ptr := To_Tuple (A);
      PyParams     : PyObject_Ptr;
   begin
      Assert (CLF /= System.Null_Address, Routine_Name & "CLF is null");
      Assert (A_Tuple /= Null_Address, Routine_Name & "A_Tuple is null");

      PyParams :=
        Py_BuildValue (Interfaces.C.To_C ("OO"), CLF, A_Tuple);
      Assert (PyParams /= Null_Address, Routine_Name & "PyParams is null");

      CLF := Call_Object (PyFunc, PyParams);
      if CLF = System.Null_Address then
         Put (Routine_Name & "Py error message: ");
         PyErr_Print;
      end if;

      Py_DecRef (PyFunc);
      Py_DecRef (A_Tuple);
      Py_DecRef (PyParams);

   end Call;

   --  -------------------------------------------------------------------------

   procedure Call (M   : Python.Module; Function_Name : String;
                   CLF : PyObject_Ptr; A : Real_Float_Matrix;
                   B   : Integer_Array) is
      use Python;

      function Py_BuildValue (Format     : Interfaces.C.char_array;
                              O1, T1, T2 : PyObject_Ptr) return PyObject_Ptr;
      pragma Import (C, Py_BuildValue, "Py_BuildValue");

      Routine_Name : constant String :=
        "Python_CLF.Call Float_Matrix, Integer_Array ";
      F            : constant PyObject_Ptr := Get_Symbol (M, Function_Name);
      A_Tuple      : constant PyObject_Ptr := To_Tuple (A);
      B_Tuple      : constant PyObject_Ptr := To_Tuple (B);
      PyParams     : PyObject_Ptr;
      PyResult     : PyObject_Ptr;
   begin
      Assert (CLF /= System.Null_Address, Routine_Name & "CLF is null");
      PyParams :=
        Py_BuildValue (Interfaces.C.To_C ("OOO"), CLF, A_Tuple, B_Tuple);
      PyResult := Call_Object (F, PyParams);

      if PyResult = System.Null_Address then
         Put (Routine_Name & "Py error message: ");
         PyErr_Print;
      end if;

      Py_DecRef (F);
      Py_DecRef (A_Tuple);
      Py_DecRef (B_Tuple);
      Py_DecRef (PyParams);
      Py_DecRef (PyResult);

   end Call;

   --  -------------------------------------------------------------------------

   procedure Call (M   : Python.Module; Function_Name : String;
                   CLF : PyObject_Ptr;
                   A : Real_Float_Vector; B : Integer_Array) is
      use Python;

      function Py_BuildValue (Format     : Interfaces.C.char_array;
                              O1, T1, T2 : PyObject_Ptr) return PyObject_Ptr;
      pragma Import (C, Py_BuildValue, "Py_BuildValue");

      Routine_Name : constant String :=
        "Python_CLF.Call Float_Matrix, Integer_Array ";
      F            : constant PyObject_Ptr := Get_Symbol (M, Function_Name);
      A_Tuple      : constant PyObject_Ptr := To_Tuple (A);
      B_Tuple      : constant PyObject_Ptr := To_Tuple (B);
      PyParams     : PyObject_Ptr;
      PyResult     : PyObject_Ptr;
   begin
      Assert (CLF /= System.Null_Address, Routine_Name & "CLF is null");
      PyParams :=
        Py_BuildValue (Interfaces.C.To_C ("OOO"), CLF, A_Tuple, B_Tuple);
      PyResult := Call_Object (F, PyParams);

      if PyResult = System.Null_Address then
         Put (Routine_Name & "Py error message: ");
         PyErr_Print;
      end if;

      Py_DecRef (F);
      Py_DecRef (A_Tuple);
      Py_DecRef (B_Tuple);
      Py_DecRef (PyParams);
      Py_DecRef (PyResult);

   end Call;

   --  -------------------------------------------------------------------------

   function Call (M   : Python.Module; Function_Name : String;
                  CLF : PyObject_Ptr;
                  A  : Real_Float_Matrix; B  : Integer_Array) return Float is
      use Python;
      Routine_Name : constant String := "Python_CLF.Call RFMI ";

      function Py_BuildValue (Format     : Interfaces.C.char_array;
                              O1, T1, T2 : PyObject_Ptr) return PyObject_Ptr;
      pragma Import (C, Py_BuildValue, "Py_BuildValue");

      F        : constant PyObject_Ptr := Get_Symbol (M, Function_Name);
      A_Tuple  : constant PyObject_Ptr := To_Tuple (A);
      B_Tuple  : constant PyObject_Ptr := To_Tuple (B);
      PyParams : PyObject_Ptr;
      PyResult : PyObject_Ptr;
      Result   : Float;
   begin
      Assert (CLF /= System.Null_Address, Routine_Name & "CLF is null");
      PyParams :=
        Py_BuildValue (Interfaces.C.To_C ("OOO"), CLF, A_Tuple, B_Tuple);
      PyResult := Call_Object (F, PyParams);
      Result := Float (PyFloat_AsDouble (PyResult));

      Py_DecRef (F);
      Py_DecRef (A_Tuple);
      Py_DecRef (B_Tuple);
      Py_DecRef (PyParams);
      Py_DecRef (PyResult);

      return Result;

   end Call;

   --  -------------------------------------------------------------------------

   function Call (M : Python.Module; Function_Name : String;
                  A : Real_Float_Matrix; B : Integer_Array)
                  return PyObject_Ptr is
      use Interfaces.C;

      function Py_BuildValue (Format : char_array; T1, T2 : PyObject_Ptr)
                              return PyObject_Ptr;
      pragma Import (C, Py_BuildValue, "Py_BuildValue");

      F            : constant PyObject_Ptr :=
        Python.Get_Symbol (M, Function_Name);
      A_Tuple      : constant PyObject_Ptr := To_Tuple (A);
      B_Tuple      : constant PyObject_Ptr := To_Tuple (B);
      PyParams     : PyObject_Ptr;
      PyResult     : PyObject_Ptr;
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

   procedure Call (M   : Python.Module; Function_Name : String;
                   CLF : PyObject_Ptr; A : ML_Types.Unbounded_List) is
      use Python;

      function Py_BuildValue (Format : Interfaces.C.char_array;
                              O1, T1 : PyObject_Ptr) return PyObject_Ptr;
      pragma Import (C, Py_BuildValue, "Py_BuildValue");

      Routine_Name : constant String :=
        "Python_CLF.Call Float_Matrix, Integer_Array ";
      F            : constant PyObject_Ptr := Get_Symbol (M, Function_Name);
      A_Tuple      : constant PyObject_Ptr := To_Tuple (A);
      PyParams     : PyObject_Ptr;
      PyResult     : PyObject_Ptr;
   begin
      Assert (CLF /= System.Null_Address, Routine_Name & "CLF is null");
      PyParams :=
        Py_BuildValue (Interfaces.C.To_C ("OO"), CLF, A_Tuple);
      PyResult := Call_Object (F, PyParams);

      if PyResult = System.Null_Address then
         Put (Routine_Name & "Py error message: ");
         PyErr_Print;
      end if;

      Py_DecRef (F);
      Py_DecRef (A_Tuple);
      Py_DecRef (PyParams);
      Py_DecRef (PyResult);

   end Call;

   --  -------------------------------------------------------------------------

   function Call (M   : Python.Module; Function_Name : String;
                  CLF : PyObject_Ptr; A : ML_Types.Unbounded_List)
                  return ML_Types.Integer_List is
      use Python;

      function Py_BuildValue (Format : Interfaces.C.char_array;
                              O1, T1 : PyObject_Ptr) return PyObject_Ptr;
      pragma Import (C, Py_BuildValue, "Py_BuildValue");

      Routine_Name : constant String :=
        "Python_CLF.Call Float_Matrix, Integer_Array ";
      F            : constant PyObject_Ptr := Get_Symbol (M, Function_Name);
      A_Tuple      : constant PyObject_Ptr := To_Tuple (A);
      PyParams     : PyObject_Ptr;
      PyResult     : PyObject_Ptr;
      Result       : ML_Types.Integer_List;
   begin
      Assert (CLF /= System.Null_Address, Routine_Name & "CLF is null");
      PyParams :=
        Py_BuildValue (Interfaces.C.To_C ("OO"), CLF, A_Tuple);
      PyResult := Call_Object (F, PyParams);

      if PyResult = System.Null_Address then
         Put (Routine_Name & "Py error message: ");
         PyErr_Print;
      end if;

      Py_DecRef (F);
      Py_DecRef (A_Tuple);
      Py_DecRef (PyParams);

      Result := Parsers.Parse_Tuple (PyResult);

      Py_DecRef (PyResult);

      return Result;

   end Call;

   --  -------------------------------------------------------------------------

   function Get_Attribute (CLF : PyObject_Ptr; Attribute : String)
                           return PyObject_Ptr is
      use Interfaces.C;
      Routine_Name : constant String :=
        "Python_CLF.Get_Attribute Real_Float_Matrix ";
      PyString     : constant PyObject_Ptr :=
        PyString_FromString (To_C (Attribute));
      Py_Result    : PyObject_Ptr;
   begin
      Assert (CLF /= Null_Address, Routine_Name & "CLF is null");
      Py_Result := PyObject_GetAttr (CLF, PyString);
      if Py_Result = System.Null_Address then
         Put (Routine_Name & "Py error message: ");
         PyErr_Print;
      end if;

      return Py_Result;

   end Get_Attribute;

   --  -------------------------------------------------------------------------

end Python_CLF;
