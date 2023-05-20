--
--  The evaluation of a game state where the player has a choice is the
--  maximum evaluation of the game states that can be reached in one move.
--  A good estimate for those game states may not have been obtained yet but
--  updates can be continued to make the game tree values more consistent from
--  one step of the game to the next.
--  The basic idea of the Q-learning approach is as follows:
--  1. Let Q(S,M) be a function that maps a game state S and an action M to
--  an initally, possibly random, value that represents the probability that

--  after that.
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

--  with Basic_Printing; use  Basic_Printing;
with ML_Arrays_And_Matrices; use ML_Arrays_And_Matrices;
with ML_Types;
with Python;
with Python_API;
with Python_Class;

with Support_13A; use Support_13A;

procedure Lesson_13A is
   use System;
   Program_Name     : constant String := "Lesson 13A ";
   --  100,000 rounds makes sure that the data contains rare but important
   --  events
   Rounds           : constant Positive := 10000;
   Epochs           : constant Positive := 4;
   Epsilon          : constant Float := 0.1;
   Classifier       : Python.Module;
   Env              : Python_API.PyObject;
   CLF              : Python_Class.PyTypeObject :=
                        System.Null_Address;
--     Graph            : Python_Class.PyTypeObject;
   Labels           : ML_Types.Integer_List;
   Action           : Natural := 0;
   Observation      : Real_Float_Vector (1 .. 2) := (0.0, 0.0);
   Data             : Float_Vector_List;
   Data_Item        : Real_Float_Vector (1 .. Observation'Length + 1);
   Reward           : Float;
   Target           : Float;
   Wins             : Natural;
   Done             : Boolean;
begin
   Put_Line (Program_Name);
   Python.Initialize;

   Classifier := Python.Import_File ("lesson_13a");
   Env := Python.Call (Classifier, "init_gym", "Blackjack-v1");

   for epoch in 0 .. Epochs loop
      --  Gather a set of training examples consisting of the current game
      --  state in Data and an improved estimate of its value from the
      --  Q-update equation in Labels.
      Put_Line (Program_Name & "epoch: " & Integer'Image (epoch));
      Data.Clear;
      Labels.Clear;
      Wins := 0;
      for round in 1 .. Rounds loop
         --  For each of round, we start off with an observation of the
         --  initial game state which, in blackjack, comes from dealing one
         --  card to the dealer and two to the player.
         Done := False;
         Python.Call (Classifier, "reset", Env);
         while not Done loop
            --  Ask the learner to pick an action.
            Action :=
              Action_Picker (Classifier, Env, CLF, Observation, Epsilon);
            --  Add the current game state and the current action to the
            --  training data.
            Data_Item := (Observation (1), Observation (2), Float (Action));
            Data.Append (Data_Item);
            --  Take a step in the environment following the selected action.

            Done := Call (Classifier, "step", Env, Action, Observation, Reward);

            if Done then
               Target := Reward;
            elsif epoch = 0 then
               Target := 0.0;
            else
               Assert (CLF /= Null_Address, Program_Name & "CLF is null!");
               Data_Item := (Observation (1), Observation (2), 1.0);
               declare
                  Predictions : constant Real_Float_Vector := Python_Class.Call
                    (Classifier, "predict", Clf, To_Real_Float_Matrix (Data));
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

      --  retrain evaluation function
      CLF :=  Python_Class.Call (Classifier, "train", Data, Labels);

--        Graph := Python_Class.Call (Classifier, "graph", Clf);
      Python.Call (Classifier, "close", Env);
      Put_Line (Program_Name & "trained with data size" &
                  Integer'Image (Integer (Data.Length)) & " x" &
                  Integer'Image (Data.First_Element'Length));
      Put_Line (Program_Name & "wins:" & Integer'Image (Wins));
   end loop;

   Put_Line (Program_Name & "All epochs completed.");

--     Python_Class.Call (Classifier, "show_graph", Graph);
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

end Lesson_13A;
