
--  The evaluation of a game state where the player has a choice is the
--  maximum evaluation of the game states that can be reached in one move.
--  A good estimate for those game states may not have been obtained yet but
--  updates can be continued to make the game tree values more consistent from
--  one step of the game to the next.
--  The basic idea of the Q-learning approach is as follows:
--  1. Let Q(S,M) be a function that maps a game state S and an action M to
--  an initally, possibly random, value that represents the probability that
--
--  2. Then, each time the learner sees that the game state S leads to a game
--  state S-prime when move M is taken, it uses its estimate of the value of
--  the best move to take from S-prime to update Q(S,M).
--  A decision tree is used to represent Q and is retrained periodically as
--  more data becomes available.
--  An important parameter is epsilon.
--  To ensure that we get a chance to see how other moves work, choose a
--  random move epsilon equals 10% of the time.
--  Epsilon strikes a tradeoff between exploring and exploiting.

with System;

with Ada.Assertions; use Ada.Assertions;
with Ada.Exceptions; use Ada.Exceptions;
with Ada.Text_IO; use Ada.Text_IO;

with Maths;

--  with Basic_Printing; use  Basic_Printing;
with ML_Arrays_And_Matrices; use ML_Arrays_And_Matrices;
--  with ML_Types;
with Python;
with Python_API;
with Python_Class;

with Support_13QS; use Support_13QS;

procedure Lesson_13QS is
   use System;
   Program_Name     : constant String := "Lesson 13QS ";
   --  100,000 rounds makes sure that the data contains rare but important
   --  events
   Rounds           : constant Positive := 10000;
   Epochs           : constant Positive := 4;
   Gamma            : constant Float := 0.9;
   Epsilon          : constant Float := 0.1;
   Classifier       : Python.Module;
   Env              : Python_API.PyObject;
   CLF              : Python_Class.PyClass :=
                        System.Null_Address;
   Labels           : Real_Float_List;
   Action           : Natural;
   Current_State    : Integer := 0;
   Data             : Integer_Array_List;
   Data_Item        : Integer_Array (1 .. 2);
   Reward           : Integer; --  Win 1, Lose, -1, Draw 0
   Ret              : Integer;
   Target           : Float;
   Done             : Boolean;
begin
   Put_Line (Program_Name);
   Python.Initialize;

   Classifier := Python.Import_File ("lesson_13qs");
   Env := Python.Call (Classifier, "init_gym", "Roulette-v0");

   for epoch in 0 .. Epochs loop
      --  Gather a set of training examples consisting of the current game
      --  state in Data and an improved estimate of its value from the
      --  Q-update equation in Labels.
      New_Line;
      Put_Line (Program_Name & "epoch: " & Integer'Image (epoch));
      Data.Clear;
      Labels.Clear;

      for round in 1 .. Rounds loop
         Done := False;
         Ret := 0;
         Current_State := Python.Call (Classifier, "reset", Env);

         while not Done loop
            if epoch = 0 then
               Action := Python.Call (Classifier, "action", Env);
            else
               Assert (CLF /= Null_Address, Program_Name & "CLF is null!");
               declare
                  Predictions : constant Real_Float_Vector := Python_Class.Call
                    (Classifier, "predict", Clf, Current_State);
               begin
                  --  Negative predictions imply loss
                  Action := Support_13QS.Arg_Max (Predictions);
               end;
            end if;

            if Maths.Random_Float < Epsilon then
               Action := Python.Call (Classifier, "action", Env);
            end if;

            Data_Item := (Current_State, Action);
            Data.Append (Data_Item);

            --  Take a step in the environment following the selected action.
            Done := Step (Classifier, "step", Env, Action,
                          Current_State, Reward);
            Ret := Ret + Reward;

            if Done then
               Target := Float (Reward);
            elsif epoch = 0 then
               Target := 0.0;
            else
               declare
                  Predictions : constant Real_Float_Vector := Python_Class.Call
                    (Classifier, "predict", Clf, Current_State);
               begin
                  --  Negative predictions imply loss
                  Target := Float (Reward) +
                    Gamma * Support_13QS.Max (Predictions);
               end;

               Labels.Append (Target);
            end if;
         end loop;
      end loop;

      --  retrain evaluation function
      CLF :=  Python_Class.Call (Classifier, "train", Data, Labels);

      Python.Call (Classifier, "close", Env);
      Put_Line (Program_Name & "trained with data size" &
                  Integer'Image (Integer (Data.Length)) & " x" &
                  Integer'Image (Data.First_Element'Length));
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

end Lesson_13QS;