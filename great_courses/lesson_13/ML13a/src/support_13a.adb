
with Interfaces.C;

with Ada.Assertions; use Ada.Assertions;
with Ada.Text_IO; use Ada.Text_IO;

with Maths;

--  with Basic_Printing; use Basic_Printing;
with Python_Class;

package body Support_13A is

   function Action_Picker (Classifier  : Python.Module;
                           Env         : Python_API.PyObject;
                           CLF         : Python_Class.PyTypeObject :=
                             System.Null_Address;
                           Observation : Real_Float_Vector;
                           Epsilon     : Float) return Natural is
      use System;
      Routine_Name : constant String := "Support_13a.Action_Picker ";
      Examples     : Real_Float_Matrix (1 .. 2, 1 .. Observation'Length + 1);
      Action       : Integer;
   begin
      if CLF = Null_Address then
         Action := Python.Call (Classifier, "sample", Env);
      else
         Assert (CLF /= Null_Address, Routine_Name & "CLF is null!");

         for col in Observation'Range loop
            Examples (1, col) := Observation (col);
            Examples (2, col) := Observation (col);
         end loop;
         Examples (1, Examples'Last (2)) := 0.0;
         Examples (2, Examples'Last (2)) := 1.0;

         Put_Line (Routine_Name & "calling predict ");
         declare
            Predictions : constant Real_Float_Matrix :=
                            Python_Class.Call (Classifier, "predict", Clf,
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
                  Observation : out Real_Float_Vector; Reward : out Float)
                  return Boolean is
      use Interfaces.C;
      use Python;
      use Python_API;

      function Parse_Tuple (Tuple       : PyObject;
                            Observation : out Real_Float_Vector;
                            Reward      : out Float) return Boolean is
         Py_Obs : PyObject;
      begin
--           Assert (Integer (PyTuple_Size (Tuple)) = 4,
--                   "Parse_Tuple Tuple Size" & int'Image (PyTuple_Size (Tuple)) &
--                     ", size 4 expected");
         Py_Obs := PyTuple_GetItem (Tuple, 0);
         Observation (1) :=
           Float (PyFloat_AsDouble (PyTuple_GetItem (Py_Obs, 0)));
         Observation (2) :=
           Float (PyFloat_AsDouble (PyTuple_GetItem (Py_Obs, 1)));
         Reward := Float (PyFloat_AsDouble (PyTuple_GetItem (Tuple, 1)));

         return Float (PyFloat_AsDouble (PyTuple_GetItem (Tuple, 2))) /= 0.0;

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

   function Max (Values : Real_Float_Matrix) return Float is
      Max_Value : Float := Values (Values'First, 1);
   begin
      for row in Values'Range loop
         if Values (row, 1) > Max_Value then
            Max_Value := Values (row, 1);
         end if;
      end loop;

      return Max_Value;

   end Max;

   --  -------------------------------------------------------------------------

--     function Train (Classifier : Python.Module; Data : Float_Array_List;
--                     Labels     : ML_Types.Integer_List)
--                     return Python_API.PyObject is
--        use System;
--        Routine_Name : constant String := "Support_13a.Train ";
--        Max_Leaves : constant Positive := 6;
--        CLF        : constant Python_API.PyObject := Python_CLF.Call
--          (Classifier, "init_decision_tree_regressor", Max_Leaves);
--     begin
--        Assert (CLF /= Null_Address, Routine_Name & "CLF is null!");
--        Python_CLF.Call (Classifier, "fit", CLF, Data, Labels);
--
--        return CLF;
--
--     end Train;

   --  -------------------------------------------------------------------------

end Support_13A;
