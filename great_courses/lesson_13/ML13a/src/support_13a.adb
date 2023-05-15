
with Interfaces.C;

with Ada.Assertions; use Ada.Assertions;
--  with Ada.Containers;
--  with Ada.Text_IO; use Ada.Text_IO;
--
with Maths;

--  with Basic_Printing; use Basic_Printing;
--  with Neural_Utilities;
with Python_CLF;
--  with Tuple_Builder;

package body Support_13A is

   function Action_Picker (Classifier  : Python.Module;
                           Env         : Python_API.PyObject;
                           CLF         : Python_API.PyObject :=
                             System.Null_Address;
                           Observation : Real_Float_Vector;
                           Epsilon     : Float) return Natural is
      pragma Unreferenced (Observation);
      use System;
      --        Routine_Name : constant String := "Support_13a.Action_Picker ";
      Examples     : Integer_Array_List;
      Action       : Integer;
   begin
      if CLF = Null_Address then
         Action := Python.Call (Classifier, "sample", Env);
      else
         declare
            Predictions : constant Real_Float_Matrix :=
                            Python_CLF.Call (Classifier, "predict", Clf,
                                             Examples);
         begin
            if Predictions (2, 1) > Predictions (1, 1) then
               Action := 1;
            else
               Action := 0;
            end if;
         end;
      end if;

      --  Random_Float range 0.0 .. 1.0
      if Maths.Random_Float < Epsilon then
         Action := Python.Call (Classifier, "sample", Env);
      end if;

      return Action;

   end Action_Picker;

   --  -------------------------------------------------------------------------

   function Call (M           : Python.Module; Function_Name : String;
                  Env         : Python_API.PyObject; Action : Integer;
                  Observation : out Real_Float_Vector; Reward : out Integer)
                  return Boolean is
      use Interfaces.C;
      use Python;
      use Python_API;

      function Parse_Tuple (Tuple       : PyObject;
                            Observation : out Real_Float_Vector;
                            Reward      : out Integer) return Boolean is
         Py_Obs : PyObject;
      begin
         Assert (Integer (PyTuple_Size (Tuple)) = 4,
                 "Parse_Tuple Tuple Size" & int'Image (PyTuple_Size (Tuple)) &
                   ", size 4 expected");
         Py_Obs := PyTuple_GetItem (Tuple, 0);
         Observation (1) :=
           Float (PyFloat_AsDouble (PyTuple_GetItem (Py_Obs, 0)));
         Observation (2) :=
           Float (PyFloat_AsDouble (PyTuple_GetItem (Py_Obs, 1)));
         Reward := Integer (PyLong_AsLong (PyTuple_GetItem (Tuple, 1)));

         return Integer (PyLong_AsLong (PyTuple_GetItem (Tuple, 2))) /= 0;

      end Parse_Tuple;

      function Py_BuildValue (Format : Interfaces.C.char_array;
                              T1     : PyObject; I1 : int) return PyObject;
      pragma Import (C, Py_BuildValue, "Py_BuildValue");

      F        : constant PyObject := Get_Symbol (M, Function_Name);
      PyParams : PyObject;
      PyResult : PyObject;
      Result   : Boolean;
   begin
      PyParams :=
        Py_BuildValue (Interfaces.C.To_C ("Oi"), Env, int (Action));

      PyResult := Call_Object (F, PyParams);
      Result := Parse_Tuple (PyResult, Observation, Reward);

      Py_DecRef (F);
      Py_DecRef (PyParams);
      Py_DecRef (PyResult);

      return Result;

   end Call;

   --  -------------------------------------------------------------------------

   function Train (Classifier : Python.Module; Data : ML_Types.Integer_List_2D;
                   Labels     : ML_Types.Integer_List)
                   return Python_API.PyObject is
      Max_Leaves : constant Positive := 6;
      CLF        : Python_API.PyObject := Python_CLF.Call
        (Classifier, "init_decision_tree_regressor", Max_Leaves);
   begin

      return Python_CLF.Call
        (Classifier, "fit", CLF, Data, Labels);

   end Train;

   --  -------------------------------------------------------------------------

end Support_13A;
