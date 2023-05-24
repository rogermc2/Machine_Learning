
with System;

with Ada.Assertions; use Ada.Assertions;
with Ada.Exceptions; use Ada.Exceptions;
with Ada.Text_IO; use Ada.Text_IO;

--  with Basic_Printing; use  Basic_Printing;
with ML_Arrays_And_Matrices; use ML_Arrays_And_Matrices;
with ML_Types;
with Python;
with Python_API;
with Python_Class;

with Support_15A; use Support_15A;

procedure Lesson_15A is
   use System;
   Program_Name     : constant String := "Lesson 15A ";

   Cats_1 : String_9_array (1 .. 36) :=
     ("n01443537", "n01629819", "n01641577", "n01644900", "n01698640", "n01742172",
      "n01855672", "n01882714", "n02002724", "n02056570", "n02058221", "n02074367",
      "n02085620", "n02094433", "n02099601", "n02099712", "n02106662", "n02113799",
      "n02123045", "n02123394", "n02124075", "n02125311", "n02129165", "n02132136",
      "n02364673", "n02395406", "n02403003", "n02410509", "n02415577", "n02423022",
      "n02437312", "n02480495", "n02481823", "n02486410", "n02504458", "n02509815");
   Cats_2  : String_9_array (1 .. 14) :=
     ("n01770393", "n01774384", "n01774750", "n01784675", "n02165456", "n02190166",
      "n02206856", "n02226429", "n02231487", "n02233338", "n02236044", "n02268443",
      "n02279972", "n02281406");
   --     Train_Data       : constant Data_Record :=
   --                          Get_Data ("../../imgs/tiny-imagenet-200/train/
   --     Test_Data        : constant Data_Record :=
   --                          Get_Data ("../../imgs/tiny-imagenet-200/test/");
   Rounds           : constant Positive := 10000;
   Epochs           : constant Positive := 4;
   Epsilon          : constant Float := 0.1;
   Classifier       : Python.Module;
   Env              : Python_API.PyObject;
   CLF              : Python_Class.PyClass :=
     System.Null_Address;
   Labels           : ML_Types.Integer_List;
   Action           : Boolean := False;
   Int_Action       : Natural := 0;
   Current_State    : Integer_Array (1 .. 3) := (0, 0, 0);
   Data             : Integer_Array_List;
   Test             : Integer_Matrix  (1 .. 2, 1 .. 3);
   Data_Item        : Integer_Array (Current_State'Range);
   Reward           : Integer; --  Win 1, Lose, -1, Draw 0
   Target           : Integer;
   Wins             : Natural;
   Count            : Natural;
   Done             : Boolean;
begin
   Put_Line (Program_Name);
   Python.Initialize;

   Classifier := Python.Import_File ("lesson_15a");
   Env := Python.Call (Classifier, "init_gym", "Blackjack-v1");

   for epoch in 0 .. Epochs loop
      New_Line;
      Put_Line (Program_Name & "epoch: " & Integer'Image (epoch));
      Data.Clear;
      Labels.Clear;
      Wins := 0;
      Count := 0;
      for round in 1 .. Rounds loop
         Done := False;
         Current_State := Python.Call (Classifier, "reset", Env);
         while not Done loop
            Action :=
              Action_Picker (Classifier, Env, CLF, Current_State, Epsilon);
            if Action then
               Int_Action := 1;
            else
               Int_Action := 0;
            end if;

            Data_Item := (Current_State (1), Current_State (2), Int_Action);
            Data.Append (Data_Item);
            --  Take a step in the environment following the selected action.
            Done := Step (Classifier, "step", Env, Action,
                          Current_State, Reward);

            if Done then
               Target := Reward;
            elsif epoch = 0 then
               Target := 0;
            else
               Assert (CLF /= Null_Address, Program_Name & "CLF is null!");
               --  Ask the classifier what it predicts from the current board
               --  combined with both of the two possible actions.
               Test := ((Current_State (1), Current_State (2), 0),
                        (Current_State (1), Current_State (2), 1));
               declare
                  Predictions : constant Real_Float_Vector := Python_Class.Call
                    (Classifier, "predict", Clf, Test);
               begin
                  --  Negative predictions imply loss
                  Target := Integer (Support_15A.Max (Predictions));
               end;
            end if;
            Labels.Append (Target);

            Count := Count + 1;
            if Reward > 0 then
               Wins := Wins + 1;
            end if;
         end loop;
      end loop;

      --  retrain evaluation function
      CLF :=  Python_Class.Call (Classifier, "train", Data, Labels);

      Python.Call (Classifier, "close", Env);
      Put_Line (Program_Name & "trained with data size" &
                  Integer'Image (Integer (Data.Length)) & " x" &
                  Integer'Image (Data.First_Element'Length));
      Put_Line (Program_Name & "wins:" &
                  Float'Image (100.0 * Float (Wins) / Float (Count)) & "%");
   end loop;

   Put_Line (Program_Name & "All epochs completed.");
   Python.Call (Classifier, "plot", CLF);
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

end Lesson_15A;
