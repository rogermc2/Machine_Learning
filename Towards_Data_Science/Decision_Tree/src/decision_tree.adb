--  https://towardsdatascience.com/decision-tree-in-machine-learning-e380942a4c96

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;

with Builder;
with ML_Types; use ML_Types;
with Support; use Support;

procedure Decision_Tree is
   Num_Features : constant Positive := 2;

   Training_Data : constant Row_Array (1 .. 5) :=
                             --  Colour Diameter Label
                     ((Num_Features, Green, 3, Apple),
                      (Num_Features, Yellow, 3, Apple),
                      (Num_Features, Red, 1, Grape),
                      (Num_Features, Red, 1, Grape),
                      (Num_Features, Yellow, 3, Lemon));

   Question            : Question_Type;
   Rows                : Builder.Partitioned_Rows;
   No_Mixing           : Rows_Vector;
   Some_Mixing         : Rows_Vector;
   Lots_Of_Mixing      : Rows_Vector;
   Current_Uncertainty : Float;
   P_Rows              : Builder.Partitioned_Rows;
   Best                : Builder.Best_Split_Data;
   aTree               : Tree_Type;
   Classified          : Count_Package.Map;
   Probabilities       : Builder.Strings_List;
begin
   Builder.Print_Unique_Values (To_Vector (Training_Data), Colour_Feature);
   Builder.Print_Unique_Values (To_Vector (Training_Data), Diameter_Feature);
   New_Line;

   Builder.Print_Class_Counts (To_Vector (Training_Data));
   New_Line;

   Question := (Colour_Feature, Green);
   Print_Question (Question);
   Question := (Diameter_Feature, 2);
   Print_Question (Question);

   Rows := Builder.Partition (To_Vector (Training_Data), (Colour_Feature, Red));
   Print_Rows ("True rows: ", Rows.True_Rows);
   Print_Rows ("False rows: ", Rows.False_Rows);

   No_Mixing.Append ((Num_Features, Green, 3, Apple));
   No_Mixing.Append ((Num_Features, Yellow, 3, Apple));
   Put_Line ("No_Mixing Gini " & Float'Image (Gini (No_Mixing)));
   New_Line;

   Some_Mixing.Append ((Num_Features, Green, 3, Apple));
   Some_Mixing.Append ((Num_Features, Green, 3, Orange));
   Put_Line ("Some_Mixing Gini " & Float'Image (Gini (Some_Mixing)));

   Lots_Of_Mixing.Append ((Num_Features, Green, 3, Apple));
   Lots_Of_Mixing.Append ((Num_Features, Green, 3, Orange));
   Lots_Of_Mixing.Append ((Num_Features, Green, 3, Grape));
   Lots_Of_Mixing.Append ((Num_Features, Green, 3, Grapefruit));
   Lots_Of_Mixing.Append ((Num_Features, Green, 3, Blueberry));
   Put_Line ("Lots_Of_Mixing Gini " & Float'Image (Gini (Lots_Of_Mixing)));

   Current_Uncertainty := Gini (To_Vector (Training_Data));
   Put_Line ("Current_Uncertainty of Training_Data " &
               Float'Image (Current_Uncertainty));

   Question := (Colour_Feature, Green);
   P_Rows := Builder.Partition (To_Vector (Training_Data), Question);
   Put_Line ("Info gain Green" &
               Float'Image (Support.Information_Gain
               (P_Rows.True_Rows, P_Rows.False_Rows, Current_Uncertainty)));
   Print_Rows ("True rows: ", P_Rows.True_Rows);
   Print_Rows ("False rows: ", P_Rows.False_Rows);
   New_Line;

   Question := (Colour_Feature, Red);
   P_Rows := Builder.Partition (To_Vector (Training_Data), Question);
   Put_Line ("Info gain Red" &
               Float'Image (Support.Information_Gain
               (P_Rows.True_Rows, P_Rows.False_Rows, Current_Uncertainty)));
   Print_Rows ("True rows: ", P_Rows.True_Rows);
   Print_Rows ("False rows: ", P_Rows.False_Rows);

   Best := Builder.Find_Best_Split (To_Vector (Training_Data));
   Put_Line ("Best_Split " & Float'Image (Best.Best_Gain) & ", " &
               Feature_Type'Image (Best.Best_Question.Feature));
   if Best.Best_Question.Feature = Colour_Feature then
      Put_Line ("Best question " & Float'Image (Best.Best_Gain) & ", " &
                 Colour_Type'Image (Best.Best_Question.Colour_Value));
   else
      Put_Line ("Best question is dimension " &
                  Integer'Image (Best.Best_Question.Diameter_Value));
   end if;

   aTree := Builder.Build_Tree (To_Vector (Training_Data));
   Print_Tree (aTree);

   Put_Line ("Classify tests, Training_Data (1)");
   Classified := Builder.Classify (Training_Data (1), aTree);
   Builder.Print_Classification (Classified);
   New_Line;

   Probabilities := Builder.Print_Leaf (Classified);
   Put_Line ("Probabilities:" & To_String (Probabilities.First_Element));
   New_Line;
   Put_Line ("Classify tests, Training_Data (2)");
   Classified := Builder.Classify (Training_Data (2), aTree);
   Put_Line ("Classify tests,  Classified");
   Builder.Print_Classification (Classified);
   New_Line;
   Put_Line ("Classify tests,  Print_Leaf");
   Probabilities := Builder.Print_Leaf (Classified);
   Put_Line ("Probabilities:" & To_String (Probabilities.First_Element));
end Decision_Tree;
