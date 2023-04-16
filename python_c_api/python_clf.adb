--  Based on inspirel_ada-python_demo

with System; use System;
with Interfaces.C;

with Ada.Assertions; use Ada.Assertions;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Unchecked_Conversion;

with Basic_Printing; use Basic_Printing;
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

   procedure Call (M : Python.Module; Function_Name : String; CLF : PyObject;
                   A : ML_Types.Integer_List_2D) is
      use Interfaces.C;
      Routine_Name : constant String := "Python_CLF.Call IL2D ";

      function Parse_Tuple (Tuple : PyObject) return ML_Types.Integer_List_2D is
         Routine_Name : constant String := "Python_CLF.Call IL2D.Parse  ";
         Tuple_Size     : constant int := PyTuple_Size (Tuple);
         Tuple_Row_Size : constant int := PyTuple_Size (PyTuple_GetItem (Tuple, 1));
         Tuple_Row      : PyObject;
         Tuple_Item     : PyObject;
         Result_Row     : ML_Types.Integer_List;
         Result         : ML_Types.Integer_List_2D;
      begin
         Put_Line (Routine_Name & "Tuple_Size: " & int'Image (Tuple_Size));
         for row in 0 .. Tuple_Size - 1 loop
            Tuple_Row := PyTuple_GetItem (Tuple, row);
            Put_Line (Routine_Name & "Tuple_Row Size: " &
                        int'Image (PyTuple_Size (Tuple_Row)));
            Result_Row.Clear;
            for col in 0 .. Tuple_Row_Size - 1 loop
               Tuple_Item := PyTuple_GetItem (Tuple_Row, col);
               Put_Line (Routine_Name & "row, col, Tuple_Item: " &
                           int'Image (row) & int'Image (col) & "  " &
                           long'Image (PyLong_AsLong (Tuple_Item)));
               Result_Row.Append (Integer (PyLong_AsLong (Tuple_Item)));
            end loop;
            Print_Integer_List (Routine_Name & "Result_Row", Result_Row);
            Result.Append (Result_Row);
            Print_Integer_List_2D (Routine_Name & "row" & int'Image (row) &
                                     " Result: ", Result);
            New_Line;
         end loop;

         Print_Integer_List_2D (Routine_Name & "Result: ", Result);
         return Result;

      end Parse_Tuple;

      function Py_BuildValue (Format : char_array; O1, T1 : PyObject)
                              return PyObject;
      pragma Import (C, Py_BuildValue, "Py_BuildValue");

      F            : constant PyObject := Python.Get_Symbol (M, Function_Name);
      A_Tuple      : PyObject;
      PyParams     : PyObject;
      PyResult     : PyObject;
   begin
      Put_Line (Routine_Name & "A size:" & Integer'Image (Integer (A.Length)) & " x" &
                  Integer'Image (Integer (A (1).Length)));
      A_Tuple := To_Tuple (A);
      Print_Integer_List_2D (Routine_Name & "A ", Parse_Tuple (A_Tuple));
      Assert (F /= System.Null_Address, Routine_Name &
                "F is null");
      Assert (A_Tuple /= System.Null_Address, Routine_Name &
                "A_Tuple is null");
      PyParams := Py_BuildValue (To_C ("OO"), CLF, A_Tuple);
      Assert (PyParams /= System.Null_Address, Routine_Name &
                "PyParams is null");
      Put_Line (Routine_Name & "PyParams set");
      PyResult := Python.Call_Object (F, PyParams);

      Py_DecRef (F);
      Py_DecRef (A_Tuple);
      Py_DecRef (PyParams);
      Py_DecRef (PyResult);
      Put_Line (Routine_Name & "done");

   end Call;

   -- --------------------------------------------------------------------------

   procedure Call (M : Python.Module; Function_Name : String; CLF : PyObject;
                   A, B : ML_Types.Integer_List_2D) is
      use Interfaces.C;
      Routine_Name : constant String := "Python_CLF.Call IL2D2 ";

      function Py_BuildValue (Format : char_array; O1, T1, T2 : PyObject)
                              return PyObject;
      pragma Import (C, Py_BuildValue, "Py_BuildValue");

      F            : constant PyObject := Python.Get_Symbol (M, Function_Name);
      A_Tuple      : constant PyObject := To_Tuple (A);
      B_Tuple      : constant PyObject := To_Tuple (B);
      PyParams     : PyObject;
      PyResult     : PyObject;
   begin
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

   function Call (M : Python.Module; Function_Name : String; CLF : PyObject;
                  A : ML_Types.Integer_List_2D) return Integer_Matrix is
      use Interfaces.C;

      function Py_BuildValue (Format : char_array; O1, T1 : PyObject)
                              return PyObject;
      pragma Import (C, Py_BuildValue, "Py_BuildValue");

      F            : constant PyObject := Python.Get_Symbol (M, Function_Name);
      A_Tuple      : constant PyObject := To_Tuple (A);
      PyParams     : PyObject;
      Py_Result    : PyObject;
   begin
      PyParams := Py_BuildValue (To_C ("OO"), CLF, A_Tuple);
      Py_Result := Python.Call_Object (F, PyParams);

      Py_DecRef (F);
      Py_DecRef (A_Tuple);
      Py_DecRef (PyParams);

      declare
         Tuple_Size     : constant int := PyTuple_Size (Py_Result);
         Tuple_Row_Size : constant int := PyTuple_Size (PyTuple_GetItem (Py_Result, 1));
         Tuple_Row      : PyObject;
         Tuple_Item     : PyObject;
         Result         : Integer_Matrix (1 .. Integer (Tuple_Size), 1 .. Integer (Tuple_Row_Size));
      begin
         --           Put_Line (Routine_Name & "Tuple_Size: " & int'Image (Tuple_Size));
         for row in 0 .. Tuple_Size - 1 loop
            Tuple_Row := PyTuple_GetItem (Py_Result, row);
            for col in 0 .. Tuple_Row_Size - 1 loop
               Tuple_Item := PyTuple_GetItem (Tuple_Row, col);
               Result (Integer (row) + 1, Integer (col) + 1) :=
                 Integer (PyLong_AsLong (Tuple_Item));
            end loop;
         end loop;

         Py_DecRef (Py_Result);

         return Result;
      end;

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

   function Call (M   : Python.Module; Function_Name : String; CLF : PyObject)
                  return Integer_Array is
      use Interfaces.C;
      use Python;

      function Py_BuildValue (Format : Interfaces.C.char_array;
                              T1     : PyObject)  return PyObject;
      pragma Import (C, Py_BuildValue, "Py_BuildValue");

      Routine_Name : constant String := "Python_CLF.Call Integer_Array ";
      PyFunc       : constant PyObject := Get_Symbol (M, Function_Name);
      PyParams     : PyObject;
      Py_Result    : PyObject;
   begin

      PyParams := Py_BuildValue (Interfaces.C.To_C ("(O)"), CLF);
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
         Py_DecRef (PyParams);
         Py_DecRef (Py_Result);

         return Result;
      end;

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

   function Call (M   : Python.Module; Function_Name : String;
                  CLF : PyObject; A : ML_Types.Integer_List)
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

   procedure Call (M   : Python.Module; Function_Name : String;
                   CLF : in out PyObject; A, B : ML_Types.Integer_List) is
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

   procedure Call (M   : Python.Module; Function_Name : String;
                   CLF : PyObject; A : Real_Float_List) is
      use Python;

      function Py_BuildValue (Format : Interfaces.C.char_array;
                              O1, T1 : PyObject)  return PyObject;
      pragma Import (C, Py_BuildValue, "Py_BuildValue");

      Routine_Name : constant String := "Python_CLF.Call RFL ";
      PyFunc       : constant PyObject := Get_Symbol (M, Function_Name);
      A_Tuple      : constant PyObject := To_Tuple (A);
      PyParams     : PyObject;
      PyResult     : PyObject;
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
                   CLF : PyObject; A : Real_Float_Matrix) is
      use Python;

      function Py_BuildValue (Format : Interfaces.C.char_array;
                              O1, T1 : PyObject)  return PyObject;
      pragma Import (C, Py_BuildValue, "Py_BuildValue");

      Routine_Name : constant String := "Python_CLF.Call RFM IM ";
      PyFunc       : constant PyObject := Get_Symbol (M, Function_Name);
      A_Tuple      : constant PyObject := To_Tuple (A);
      PyParams     : PyObject;
      PyResult     : PyObject;
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
                   CLF : PyObject; A : Real_Float_Matrix;
                   B   : Boolean_Array) is
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

   function Call (M : Python.Module; Function_Name : String; CLF : PyObject;
                  A : Real_Float_Matrix) return Boolean_Array is
      --        Routine_Name : constant String := "Python_Clf.Call FM ";
      procedure Parse_Tuple (Tuple : PyObject;
                             Vec   : in out Boolean_Array) is
         use Interfaces.C;
         T_Row : PyObject;
      begin
         Assert (Vec'Length = Integer (PyTuple_Size (Tuple)),
                 "Parse_Tuple Real_Float_List Tuple Size " &
                   int'Image (PyTuple_Size (Tuple))
                 & " /= Vec Length" & Integer'Image (Vec'Length));
         for row in 1 .. PyTuple_Size (Tuple) loop
            T_Row := PyTuple_GetItem (Tuple, row - 1);
            Vec (Integer (row)) := PyObject_IsTrue (T_Row) /= 0;
         end loop;
      end Parse_Tuple;

      function Py_BuildValue (Format : Interfaces.C.char_array;
                              O1, T1 : PyObject) return PyObject;
      pragma Import (C, Py_BuildValue, "Py_BuildValue");

      F        : constant PyObject := Python.Get_Symbol (M, Function_Name);
      A_Tuple  : constant PyObject := To_Tuple (A);
      PyParams : PyObject;
      PyResult : PyObject;
      Result   : ML_Arrays_And_Matrices.Boolean_Array (A'Range);
   begin
      PyParams :=
        Py_BuildValue (Interfaces.C.To_C ("OO"), CLF, A_Tuple);

      PyResult := Python.Call_Object (F, PyParams);
      Parse_Tuple (PyResult, Result);

      Py_DecRef (F);
      Py_DecRef (A_Tuple);
      Py_DecRef (PyParams);
      Py_DecRef (PyResult);

      return Result;

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

   function Call (M   : Python.Module; Function_Name : String;
                  CLF : PyObject; A : Real_Float_Matrix;
                  B   : Integer_Matrix) return Float is
      Routine_Name : constant String := "Python_Clf.Call FIM float ";

      function Py_BuildValue (Format     : Interfaces.C.char_array;
                              O1, T1, T2 : PyObject) return PyObject;
      pragma Import (C, Py_BuildValue, "Py_BuildValue");

      F        : constant PyObject := Python.Get_Symbol (M, Function_Name);
      A_Tuple  : constant PyObject := To_Tuple (A);
      B_Tuple  : constant PyObject := To_Tuple (B);
      PyParams : PyObject;
      PyResult : PyObject;
      Result   : Float;
   begin
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
