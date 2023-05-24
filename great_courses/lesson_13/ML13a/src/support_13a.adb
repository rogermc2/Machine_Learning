
with Interfaces.C;

with Ada.Assertions; use Ada.Assertions;
--  with Ada.Text_IO; use Ada.Text_IO;

with Maths;

--  with Basic_Printing; use Basic_Printing;
with Python_Class;

package body Support_13A is

   function Action_Picker (Classifier  : Python.Module;
                           Env         : Python_API.PyObject;
                           CLF         : Python_Class.PyClass :=
                             System.Null_Address;
                           Observation : Integer_Array;
                           Epsilon     : Float) return Boolean is
      use System;
      Routine_Name : constant String := "Support_13a.Action_Picker ";
      Examples     : Integer_Matrix (1 .. 2, Observation'Range);
      Action       : Boolean;
   begin
      if CLF = Null_Address then
         Action := Python.Call (Classifier, "sample", Env);
      else
         Assert (CLF /= Null_Address, Routine_Name & "CLF is null!");

         for col in Observation'Range loop
            Examples (1, col) := Observation (col);
            Examples (2, col) := Observation (col);
         end loop;
         Examples (1, Examples'Last (2)) := 0;
         Examples (2, Examples'Last (2)) := 1;

         declare
            Predictions : constant Integer_Array :=
                            Python_Class.Call (Classifier, "predict", Clf,
                                               Examples);
         begin
            Action := Predictions (2) > Predictions (1);
         end;
      end if;

      --  Random_Float range 0.0 .. 1.0
      if Maths.Random_Float < Epsilon then
         Action := Python.Call (Classifier, "sample", Env);
      end if;

      return Action;

   end Action_Picker;

   --  -------------------------------------------------------------------------

   function Step (M          : Python.Module; Function_Name : String;
                  Env        : Python_API.PyObject; Action : Boolean;
                  Next_State : out Integer_Array; Reward : out Integer)
                  return Boolean is
      use Interfaces.C;
      use Python;
      use Python_API;
      --        Routine_Name : constant String := "Support_13a.Step ";

      function Parse_Tuple (Tuple  : PyObject; State : out Integer_Array;
                            Reward : out Integer) return Boolean is
         Py_Obs : PyObject;
      begin
         --  step returns next_state, reward, terminated, truncated , info
         Py_Obs := PyTuple_GetItem (Tuple, 0);
         State (1) :=
           Integer (PyInt_AsLong (PyTuple_GetItem (Py_Obs, 0)));
         State (2) :=
           Integer (PyInt_AsLong (PyTuple_GetItem (Py_Obs, 1)));
         State (3) :=
           Integer (PyInt_AsLong (PyTuple_GetItem (Py_Obs, 2)));

         Reward := Integer (PyFloat_AsDouble (PyTuple_GetItem (Tuple, 1)));

         return Integer (PyInt_AsLong (PyTuple_GetItem (Tuple, 2))) /= 0 or
           Integer (PyInt_AsLong (PyTuple_GetItem (Tuple, 3))) /= 0;

      end Parse_Tuple;

      function Py_BuildValue (Format : Interfaces.C.char_array;
                              T1     : PyObject; I1 : int) return PyObject;
      pragma Import (C, Py_BuildValue, "Py_BuildValue");

      F        : constant PyObject := Get_Symbol (M, Function_Name);
      PyParams : PyObject;
      PyResult : PyObject;
      PyAction : int := 0;
      Result   : Boolean;
   begin
      if Action then
         PyAction := 1;
      end if;
      PyParams :=
        Py_BuildValue (Interfaces.C.To_C ("Oi"), Env, PyAction);

      PyResult := Call_Object (F, PyParams);
      Result := Parse_Tuple (PyResult, Next_State, Reward);

      Py_DecRef (F);
      Py_DecRef (PyParams);
      Py_DecRef (PyResult);

      return Result;

   end Step;

   --  -------------------------------------------------------------------------

   function Max (Values : Real_Float_Vector) return Float is
      Max_Value : Float := Values (Values'First);
   begin
      for row in Values'Range loop
         if Values (row) > Max_Value then
            Max_Value := Values (row);
         end if;
      end loop;

      return Max_Value;

   end Max;

   --  -------------------------------------------------------------------------

end Support_13A;
