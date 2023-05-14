
--  with Ada.Assertions; use Ada.Assertions;
--  with Ada.Containers;
--  with Ada.Text_IO; use Ada.Text_IO;
--
with Maths;

--  with Basic_Printing; use Basic_Printing;
--  with Neural_Utilities;
with Python_CLF;

package body Support_13A is

   function Action_Picker (Classifier  : Python.Module;
                           Env         : Python_API.PyObject;
                           CLF         : Python_API.PyObject :=
                             System.Null_Address;
                           Observation : Positive;
                           Epsilon     : Float) return Natural is
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

end Support_13A;
