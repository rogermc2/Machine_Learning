
with System;

with Ada.Assertions; use Ada.Assertions;
with Ada.Exceptions; use Ada.Exceptions;
with Ada.Text_IO; use Ada.Text_IO;

--  with Basic_Printing; use  Basic_Printing;
with ML_Arrays_And_Matrices; use ML_Arrays_And_Matrices;
with ML_Types;
with Python;
with Python_API;
with Python_CLF;

with Support_13A; use Support_13A;

procedure Lesson_13A is
   use System;
   Program_Name     : constant String := "Lesson 13A ";
   Rounds           : constant Positive := 10000;
   Epochs           : constant Positive := 4;
   Epsilon          : constant Float := 0.1;
   Classifier       : Python.Module;
   Env              : Python_API.PyObject;
   CLF              : Python_API.PyObject :=
                        System.Null_Address;
   Data             : Float_Array_List;
   Data_Item        : Float_Array (1 .. 3);
   Labels           : ML_Types.Integer_List;
   Action           : Natural := 0;
   Observation      : Real_Float_Vector (1 .. 2) := (0.0, 0.0);
   Reward           : Float;
   Target           : Float;
   Wins             : Natural;
   Done             : Boolean;
   Env_Screen       : Python_API.PyObject;
begin
   Put_Line (Program_Name);
   Python.Initialize;

   Classifier := Python.Import_File ("lesson_13a");
   Env := Python.Call (Classifier, "init_gym", "Blackjack-v1");

   for epoch in 0 .. Epochs loop
      Data.Clear;
      Labels.Clear;
      Wins := 0;
      for count in 1 .. Rounds loop
         Done := False;
         Python.Call (Classifier, "reset", Env);
         while not Done loop
            Action :=
              Action_Picker (Classifier, Env, CLF, Observation, Epsilon);
            Data_Item := (Observation (1), Observation (2), Float (Action));
            Data.Append (Data_Item);
            Done := Call (Classifier, "step", Env, Action, Observation, Reward);

            if Done then
               Target := Reward;
            elsif epoch = 0 then
               Target := 0.0;
            else
               Assert (CLF /= Null_Address, Program_Name & "CLF is null!");
               Data_Item := (Observation (1), Observation (2), 1.0);
               Put_Line (Program_Name & "calling predict ");
               declare
                  Predictions : constant Real_Float_Matrix :=
                                  Python_CLF.Call (Classifier, "predict", Clf,
                                                   Data_Item);
               begin
                  Target := Support_13A.Max (Predictions);
               end;
            end if;
            Labels.Append (Integer (Target));

            if Reward > 0.0 then
               Wins := Wins + 1;
            end if;
         end loop;
      end loop;
--        CLF :=  Train (Classifier, Data, Labels);
      CLF :=  Python_CLF.Call (Classifier, "train", Data, Labels);
   end loop;

   Env_Screen :=  Python_CLF.Call (Classifier, "render", Env);
   Python.Call (Classifier, "plot", Env_Screen);
   New_Line;

   Python.Call (Classifier, "close", Env);
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
