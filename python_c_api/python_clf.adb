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
                  Obj : PyObject; A : Integer) return Float is
      use Interfaces.C;

      function Py_BuildValue (Format     : char_array;
                              O1         : PyObject; I1 : int)
                              return PyObject;
      pragma Import (C, Py_BuildValue, "Py_BuildValue");

      Routine_Name : constant String := "Python_CLF.Call Integers ";
      F            : constant PyObject := Python.Get_Symbol (M, Function_Name);
      PyParams     : PyObject;
      PyResult     : PyObject;
      Result       : Float;
   begin
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
                  return PyObject is
      use Interfaces.C;

      function Py_BuildValue (Format : char_array; I1, I2: int) return PyObject;
      pragma Import (C, Py_BuildValue, "Py_BuildValue");

      Routine_Name : constant String := "Python_CLF.Call Integers ";
      F            : constant PyObject := Python.Get_Symbol (M, Function_Name);
      PyParams     : PyObject;
      PyResult     : PyObject;
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
                  Obj : PyObject; A, B : Integer) return Float is
      use Interfaces.C;

      function Py_BuildValue (Format : char_array;
                              O1     : PyObject; I1, I2: int)
                              return PyObject;
      pragma Import (C, Py_BuildValue, "Py_BuildValue");

      Routine_Name : constant String := "Python_CLF.Call Integers ";
      F            : constant PyObject := Python.Get_Symbol (M, Function_Name);
      PyParams     : PyObject;
      PyResult     : PyObject;
      Result       : Float;
   begin
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

   function Call (M : Python.Module; Function_Name : String;
                  A : Integer_Array_List; B : ML_Types.Integer_List)
                  return PyObject is
      use Interfaces.C;

      function Py_BuildValue (Format : char_array; T1, T2 : PyObject)
                              return PyObject;
      pragma Import (C, Py_BuildValue, "Py_BuildValue");

      F            : constant PyObject := Python.Get_Symbol (M, Function_Name);
      A_Tuple      : constant PyObject := To_Tuple (A);
      B_Tuple      : constant PyObject := To_Tuple (B);
      PyParams     : PyObject;
      PyResult     : PyObject;
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
                   CLF : PyObject) is
      use Python;

      function Py_BuildValue (Format : Interfaces.C.char_array;
                              C1     : PyObject)  return PyObject;
      pragma Import (C, Py_BuildValue, "Py_BuildValue");

      Routine_Name : constant String := "Python_CLF.Call ";
      PyFunc       : constant PyObject := Get_Symbol (M, Function_Name);
      PyParams     : PyObject;
      Py_Result    : PyObject;
   begin

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
                  CLF : PyObject; A : Integer_Array_List)
                  return Integer_Array is
      use Interfaces.C;
      use Python;

      function Py_BuildValue (Format : Interfaces.C.char_array;
                              T1, T2 : PyObject)  return PyObject;
      pragma Import (C, Py_BuildValue, "Py_BuildValue");

      Routine_Name : constant String := "Python_CLF.Call Integer_Array_List ";
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
         --           Put_Line (Routine_Name & "Tuple_Size: " & int'Image (Tuple_Size));
         for index in 0 .. Tuple_Size - 1 loop
            Tuple_Item := PyTuple_GetItem (Py_Result, index);
            Result (Integer (index) + 1) :=
              Integer (PyLong_AsLong (Tuple_Item));
         end loop;

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
                   A   : Integer_Array_List; B : ML_Types.Integer_List) is
      use Python;

      function Py_BuildValue (Format     : Interfaces.C.char_array;
                              T1, T2, T3 : PyObject)  return PyObject;
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

   procedure Call
     (M   : Python.Module; Function_Name : String;
      CLF : PyObject; A : Real_Float_Matrix; B : Integer_Matrix) is
      use Python;

      function Py_BuildValue (Format     : Interfaces.C.char_array;
                              O1, T1, T2 : PyObject)  return PyObject;
      pragma Import (C, Py_BuildValue, "Py_BuildValue");

      Routine_Name : constant String := "Python_CLF.Call RFM IM ";
      PyFunc       : constant PyObject := Get_Symbol (M, Function_Name);
      A_Tuple      : constant PyObject := To_Tuple (A);
      B_Tuple      : constant PyObject := To_Tuple (B);
      PyParams     : PyObject;
      PyResult     : PyObject;
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

   procedure Call
     (M   : Python.Module; Function_Name : String;
      CLF : PyObject; A : Real_Float_Matrix; B : Real_Float_Vector) is
      use Python;

      function Py_BuildValue (Format     : Interfaces.C.char_array;
                              O1, T1, T2 : PyObject)  return PyObject;
      pragma Import (C, Py_BuildValue, "Py_BuildValue");

      Routine_Name : constant String := "Python_CLF.Call 2 * Real_Float_Vector ";
      PyFunc       : constant PyObject := Get_Symbol (M, Function_Name);
      A_Tuple      : constant PyObject := To_Tuple (A);
      B_Tuple      : constant PyObject := To_Tuple (B);
      PyParams     : PyObject;
      PyResult     : PyObject;
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
                  CLF : PyObject; A : Real_Float_List)
                  return Real_Float_Vector is
      use Interfaces.C;
      use Python;
      Routine_Name : constant String :=
                       "Python_CLF.Call Real_Float_List ";

      function Py_BuildValue (Format : Interfaces.C.char_array;
                              O1, T1 : PyObject)  return PyObject;
      pragma Import (C, Py_BuildValue, "Py_BuildValue");

      procedure Parse_Tuple (Tuple : PyObject;
                             Vec   : in out Real_Float_Vector) is
      begin
         Assert (Vec'Length = integer (PyTuple_Size (Tuple)), Routine_Name &
                   "Parse_Tuple Tuple Size " & int'Image (PyTuple_Size (Tuple))
                 & " /= Vec'Length" & Integer'Image (Vec'Length));
         for index in 1 .. PyTuple_Size (Tuple) loop
            Vec (Integer (index)) :=
              Float (PyFloat_AsDouble (PyTuple_GetItem (Tuple, index - 1)));
         end loop;
      end Parse_Tuple;

      PyFunc       : constant PyObject := Get_Symbol (M, Function_Name);
      A_Tuple      : constant PyObject := To_Tuple (A);
      PyParams     : PyObject;
      PyResult     : PyObject;
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

      Parse_Tuple (PyResult, Result);

      Py_DecRef (PyFunc);
      Py_DecRef (A_Tuple);
      Py_DecRef (PyParams);
      Py_DecRef (PyResult);

      return Result;

   end Call;

   --  -------------------------------------------------------------------------

   function Call (M   : Python.Module; Function_Name : String;
                  CLF : PyObject; A : Real_Float_Matrix)
                  return Real_Float_Vector is
      use Interfaces.C;
      use Python;
      Routine_Name : constant String :=
                       "Python_CLF.Call 2 * Real_Float_Vector ";

      function Py_BuildValue (Format : Interfaces.C.char_array;
                              O1, T1 : PyObject)  return PyObject;
      pragma Import (C, Py_BuildValue, "Py_BuildValue");

      procedure Parse_Tuple (Tuple : PyObject;
                             Vec   : in out Real_Float_Vector) is
      begin
         Assert (Vec'Length = integer (PyTuple_Size (Tuple)), Routine_Name &
                   "Parse_Tuple Tuple Size " & int'Image (PyTuple_Size (Tuple))
                 & " /= Vec'Length" & Integer'Image (Vec'Length));
         for index in 1 .. PyTuple_Size (Tuple) loop
            Vec (Integer (index)) := Float (PyFloat_AsDouble (PyTuple_GetItem (Tuple, index - 1)));
         end loop;
      end Parse_Tuple;

      PyFunc       : constant PyObject := Get_Symbol (M, Function_Name);
      A_Tuple      : constant PyObject := To_Tuple (A);
      PyParams     : PyObject;
      PyResult     : PyObject;
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

      Parse_Tuple (PyResult, Result);

      Py_DecRef (PyFunc);
      Py_DecRef (A_Tuple);
      Py_DecRef (PyParams);
      Py_DecRef (PyResult);

      return Result;

   end Call;

   --  -------------------------------------------------------------------------

   procedure Call (M   : Python.Module; Function_Name : String;
                   CLF : in out PyObject; A : ML_Types.Indef_String_List) is
      use Python;

      function Py_BuildValue (Format  : Interfaces.C.char_array;
                              T1, T2  : PyObject)  return PyObject;
      pragma Import (C, Py_BuildValue, "Py_BuildValue");

      Routine_Name : constant String := "Python_CLF.Call Unbounded_List ";
      PyFunc       : constant PyObject := Get_Symbol (M, Function_Name);
      A_Tuple      : constant PyObject := To_Tuple (A);
      PyParams     : PyObject;
   begin
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

   procedure Call (M : Python.Module; Function_Name : String; CLF : PyObject;
                   A : Real_Float_Matrix; B : Integer_Array) is
      use Python;

      function Py_BuildValue (Format     : Interfaces.C.char_array;
                              O1, T1, T2 : PyObject) return PyObject;
      pragma Import (C, Py_BuildValue, "Py_BuildValue");

      Routine_Name : constant String :=
                       "Python_CLF.Call Float_Matrix, Integer_Array ";
      F            : constant PyObject := Get_Symbol (M, Function_Name);
      A_Tuple      : constant PyObject := To_Tuple (A);
      B_Tuple      : constant PyObject := To_Tuple (B);
      PyParams     : PyObject;
      PyResult     : PyObject;
   begin
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

   procedure Call (M : Python.Module; Function_Name : String; CLF : PyObject;
                   A : Real_Float_Vector; B : Integer_Array) is
      use Python;

      function Py_BuildValue (Format     : Interfaces.C.char_array;
                              O1, T1, T2 : PyObject) return PyObject;
      pragma Import (C, Py_BuildValue, "Py_BuildValue");

      Routine_Name : constant String :=
                       "Python_CLF.Call Float_Matrix, Integer_Array ";
      F            : constant PyObject := Get_Symbol (M, Function_Name);
      A_Tuple      : constant PyObject := To_Tuple (A);
      B_Tuple      : constant PyObject := To_Tuple (B);
      PyParams     : PyObject;
      PyResult     : PyObject;
   begin
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

   function Call (M  : Python.Module; Function_Name : String; CLF : PyObject;
                  A  : Real_Float_Matrix; B  : Integer_Array) return Float is
      use Python;

      function Py_BuildValue (Format     : Interfaces.C.char_array;
                              O1, T1, T2 : PyObject) return PyObject;
      pragma Import (C, Py_BuildValue, "Py_BuildValue");

      F        : constant PyObject := Get_Symbol (M, Function_Name);
      A_Tuple  : constant PyObject := To_Tuple (A);
      B_Tuple  : constant PyObject := To_Tuple (B);
      PyParams : PyObject;
      PyResult : PyObject;
      Result   : Float;
   begin
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
                  return PyObject is
      use Interfaces.C;

      function Py_BuildValue (Format : char_array; T1, T2 : PyObject)
                              return PyObject;
      pragma Import (C, Py_BuildValue, "Py_BuildValue");

      F            : constant PyObject := Python.Get_Symbol (M, Function_Name);
      A_Tuple      : constant PyObject := To_Tuple (A);
      B_Tuple      : constant PyObject := To_Tuple (B);
      PyParams     : PyObject;
      PyResult     : PyObject;
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

   function Get_Attribute (CLF : PyObject; Attribute : String)
                           return PyObject is
      use Interfaces.C;
      Routine_Name : constant String :=
                       "Python_CLF.Get_Attribute Real_Float_Matrix ";
      PyString     : constant PyObject :=
                       PyString_FromString (To_C (Attribute));
      Py_Result    : PyObject;
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
