
with System;

with Interfaces.C; use Interfaces.C;
with Ada.Assertions; use Ada.Assertions;
--  with Ada.Exceptions; use Ada.Exceptions;
--  with Ada.Text_IO; use Ada.Text_IO;

--  with Basic_Printing; use Basic_Printing;
with Parsers;
with Python_API; use Python_API;
with Tuple_Builder;

package body Python_21A is

   function To_Tuple (Tensor : Support_21A.Boolean_Tensor) return PyObject_Ptr;

   --  -------------------------------------------------------------------------

   --     function Call (M : Python.Module; Function_Name : String;
   --                    Q : Support_21A.Float_Tensor) return Float is
   --        Routine_Name    : constant String := "Python_21A.Call  ";
   --
   --        function Py_BuildValue (Format : char_array; O1 : PyObject_Ptr)
   --                                return PyObject_Ptr;
   --        pragma Import (C, Py_BuildValue, "Py_BuildValue");
   --
   --        F        : constant PyObject_Ptr := Python.Get_Symbol (M, Function_Name);
   --        Q_Tuple  : constant PyObject_Ptr := To_Tuple (Q);
   --        PyParams : PyObject_Ptr;
   --        PyResult : PyObject_Ptr;
   --        Result   : Float;
   --     begin
   --        Put_Line (Routine_Name);
   --        PyParams := Py_BuildValue (To_C ("(O)"), Q_Tuple);
   --        PyResult := Python.Call_Object (F, PyParams);
   --        Py_DecRef (F);
   --        Py_DecRef (Q_Tuple);
   --        Py_DecRef (PyParams);
   --
   --        Result := Float (Python_API.PyFloat_AsDouble (PyResult));
   --        Py_DecRef (PyResult);
   --
   --        return Result;
   --
   --     end Call;

   -- --------------------------------------------------------------------------

   function Parse_Tuple (Tuple : PyObject_Ptr) return Plan_Data is
--        Routine_Name : constant String := "Parsers.Parse_Tuple Pi, Q ";
      Pi           : constant Real_Float_Matrix :=
                       Parsers.Parse_Tuple (PyTuple_GetItem (Tuple, 0));
      Q            : constant Real_Float_Matrix:=
                       Parsers.Parse_Tuple (PyTuple_GetItem (Tuple, 1));
      Result       : Plan_Data (Pi'Length, Pi'Length (2));
   begin
      Result.Policy := Pi;
      Result.Q := Q;

      return Result;

   end Parse_Tuple;

   --  -------------------------------------------------------------------------

   procedure Parse_Tuple (Tuple        : PyObject_Ptr;
                          Rk, Pi, Q, V : out Python_API.PyObject_Ptr) is
--        Routine_Name : constant String := "Parsers.Parse_Tuple Rk, Pi, Q, V ";
   begin
      Rk := PyTuple_GetItem (Tuple, 0);
      Pi := PyTuple_GetItem (Tuple, 1);
      Q := PyTuple_GetItem (Tuple, 2);
      V := PyTuple_GetItem (Tuple, 3);

   end Parse_Tuple;

   --  -------------------------------------------------------------------------

   function Plan (Classifier : Python.Module;
                  R, Pi, Q   : Python_API.PyObject_Ptr) return Plan_Data is
      use System;
      Routine_Name : constant String := "Python_21A.Plan  ";

      function Py_BuildValue (Format : char_array; O1, O2, O3 : PyObject_Ptr)
                              return PyObject_Ptr;
      pragma Import (C, Py_BuildValue, "Py_BuildValue");

      F        : constant PyObject_Ptr := Python.Get_Symbol (Classifier, "plan");
      PyParams : PyObject_Ptr;
      PyResult : PyObject_Ptr;
   begin
      Assert (R /= Null_Address, Routine_Name & "R is null");
      Assert (Pi /= Null_Address, Routine_Name & "Pi is null");
      Assert (Q /= Null_Address, Routine_Name & "Q is null");

      PyParams := Py_BuildValue (To_C ("OOO"), R, Pi, Q);
      Assert (PyParams /= Null_Address, Routine_Name & "PyParams is null");

      PyResult := Python.Call_Object (F, PyParams);
      Assert (PyResult /= Null_Address, Routine_Name & "PyResult is null");
      Py_DecRef (F);
      Py_DecRef (PyParams);

      declare
         Result : constant Plan_Data := Parse_Tuple (PyResult);
      begin
         Py_DecRef (PyResult);
         return Result;
      end;

   end Plan;

   --  -------------------------------------------------------------------------

   procedure Set_Policy (Classifier     : Python.Module;
                         Rewards        : Integer_Array;
                         Mat_Map        : Support_21A.Boolean_Tensor;
                         Mat_Transition : Support_21A.Boolean_Tensor;
                         Rk, Pi, Q, V   : out Python_API.PyObject_Ptr) is
      use Tuple_Builder;
--        Routine_Name    : constant String := "Python_21A.Set_Policy  ";

      function Py_BuildValue (Format : char_array; T1, T2, T3 : PyObject_Ptr)
                              return PyObject_Ptr;
      pragma Import (C, Py_BuildValue, "Py_BuildValue");

      F           : constant PyObject_Ptr := Python.Get_Symbol (Classifier,
                                                                "policy");
      R_Tuple     : constant PyObject_Ptr := To_Tuple (Rewards);
      Map_Tuple   : constant PyObject_Ptr := To_Tuple (Mat_Map);
      Trans_Tuple : constant PyObject_Ptr := To_Tuple (Mat_Transition);
      PyParams    : PyObject_Ptr;
      PyResult    : PyObject_Ptr;
   begin
      PyParams := Py_BuildValue (To_C ("OOO"), R_Tuple, Map_Tuple, Trans_Tuple);
      PyResult := Python.Call_Object (F, PyParams);

      Py_DecRef (F);
      Py_DecRef (R_Tuple);
      Py_DecRef (Map_Tuple);
      Py_DecRef (Trans_Tuple);
      Py_DecRef (PyParams);

      Parse_Tuple (PyResult, Rk, Pi, Q, V);
      Py_DecRef (PyResult);

   end Set_Policy;

   --  -------------------------------------------------------------------------

   function To_Tuple (Tensor : Support_21A.Boolean_Tensor)
                      return PyObject_Ptr is
      --        Routine_Name : constant String := "Python_21a.To_Tuple Boolean_Tensor ";
      Tuple        : PyObject_Ptr;
      Row_Tuple    : PyObject_Ptr;
      Col_Tuple    : PyObject_Ptr;
      Value        : long;
      Py_1         : int := -1;
      Py_2         : int;
      Py_3         : int;
   begin
      Tuple := PyTuple_New (int (Tensor'Length));
      for row in Tensor'Range loop
         Row_Tuple := PyTuple_New (int (Tensor'Length (2)));
         Py_1 := Py_1 + 1;
         Py_2 := -1;
         for col in Tensor'Range (2) loop
            Col_Tuple := PyTuple_New (int (Tensor'Length (3)));
            Py_2 := Py_2 + 1;
            Py_3 := -1;
            for item in Tensor'Range (3) loop
               Py_3 := Py_3 + 1;
               if Tensor (row, col, item) then
                  Value := 1;
               else
                  Value := 0;
               end if;

               PyTuple_SetItem (Col_Tuple, Py_3,
                                PyLong_FromLong (Value));
            end loop;
            PyTuple_SetItem (Row_Tuple, Py_2, Col_Tuple);
         end loop;
         PyTuple_SetItem (Tuple, Py_1, Row_Tuple);
      end loop;

      return Tuple;

   end To_Tuple;

   --  -------------------------------------------------------------------------

   --     function To_Tuple (Data : Support_21A.Float_Tensor)  return PyObject_Ptr is
   --        Routine_Name  : constant String := "Python_21a.To_Tuple ";
   --        Result        : constant PyObject_Ptr := PyTuple_New (int (Data'Length));
   --        Py_Tensor     : PyObject_Ptr;
   --        Py_Index      : int := -1;
   --     begin
   --        for mat in Data'Range loop
   --           Py_Index := Py_Index + 1;
   --           Py_Tensor := To_Tuple (Data);
   --           PyTuple_SetItem (Result, Py_Index, Py_Tensor);
   --        end loop;
   --
   --        return Result;
   --
   --     exception
   --        when E : others =>
   --           Put_Line (Routine_Name & "error" & Exception_Message (E));
   --           raise;
   --
   --     end To_Tuple;

   --  -------------------------------------------------------------------------

end Python_21A;
