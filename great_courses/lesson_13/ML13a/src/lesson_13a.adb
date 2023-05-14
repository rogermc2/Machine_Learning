
with System;

with Ada.Exceptions; use Ada.Exceptions;
with Ada.Text_IO; use Ada.Text_IO;

--  with Basic_Printing; use  Basic_Printing;
with ML_Arrays_And_Matrices; use ML_Arrays_And_Matrices;
with ML_Types;
with Python;
with Python_API;

with Support_13A; use Support_13A;

procedure Lesson_13A is
   Program_Name     : constant String := "Lesson 13A ";
   Rounds           : constant Positive := 10000;
   Epochs           : constant Positive := 5;
   Epsilon          : constant Float := 0.1;
   Classifier       : Python.Module;
   Env              : Python_API.PyObject;
   CLF              : Python_API.PyObject :=
                             System.Null_Address;
   Action           : Positive;
   Observation      : Real_Float_Vector (1 .. 2);
   Reward           : Positive;
   Done             : Boolean;
   Result           : ML_Types.Integer_List;
begin
   Put_Line (Program_Name);
   Python.Initialize;

   Classifier := Python.Import_File ("lesson_13a");
   Env := Python.Call (Classifier, "init_gym", "Blackjack-v1");

   for epoch in 1 .. Epochs loop
      Done := False;
      Python.Call (Classifier, "reset", Env);
      while not Done loop
         Action := Action_Picker (Classifier, Env, CLF, Observation, Epsilon);
         Done := Call (Classifier, "step", Env, Action, Observation, Reward);
      end loop;
   end loop;

--     Python.Call (Classifier, "plot", Alphas
   New_Line;

   Python.Close_Module (Classifier);
   Python.Finalize;

   Put_Line ("----------------------------------------------");

   Put_Line (Program_Name & "finished.");
   New_Line;

exception
   when Error: Constraint_Error => Put_Line (Program_Name &
                                               "Constraint_Error");
      Put_Line (Exception_Information(Error));
   when Error: others => Put_Line (Program_Name & "exception");
      Put_Line (Exception_Information(Error));

end Lesson_13A;
