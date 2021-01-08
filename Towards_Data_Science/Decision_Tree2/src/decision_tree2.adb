--  https://towardsdatascience.com/decision-tree-in-machine-learning-e380942a4c96

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
--  with Ada.Text_IO; use Ada.Text_IO;

with ML_Types2; use ML_Types2;
with Builder2; use Builder2;

procedure Decision_Tree2 is

   function UB (Source : String) return Unbounded_String renames
     To_Unbounded_String;

   Training_Data : constant Row_Array (1 .. 6) :=
                   --  Colour Diameter Label
                     (UB ("Colour, Diameter, Fruit"),
                      UB ("Green, 3, Apple"),
                      UB ("Yellow, 3, Apple"),
                      UB ("Red, 1, Grape"),
                      UB ("Red, 1, Grape"),
                      UB ("Yellow, 3, Lemon"));

   aQuestion           : Question;
--     Rows                : Partitioned_Rows;
--     No_Mixing           : Rows_Vector;
--     Some_Mixing         : Rows_Vector;
--     Lots_Of_Mixing      : Rows_Vector;
--     Current_Uncertainty : Float;
--     P_Rows              : Partitioned_Rows;
--     Best                : Best_Split_Data;
--     aTree               : Tree_Type;
--     Classified          : Count_Package.Map;
begin
--     Print_Unique_Values (To_Vector (Training_Data), Colour_Feature);
--     Print_Unique_Values (To_Vector (Training_Data), Diameter_Feature);
--     New_Line;

--     Print_Class_Counts (To_Vector (Training_Data));
--     New_Line;

   aQuestion.Feature := (UB("Colour"));
   aQuestion.Value := (UB("Green"));
   Print_Question (aQuestion);
--     aQuestion := (Diameter_Feature, 2);
--     Print_Question (Question);

--     Rows := Partition (To_Vector (Training_Data), (Colour_Feature, Red));
--     Print_Rows ("True rows: ", Rows.True_Rows);
--     Print_Rows ("False rows: ", Rows.False_Rows);
--
--     No_Mixing.Append ((Num_Features, Green, 3, Apple));
--     No_Mixing.Append ((Num_Features, Yellow, 3, Apple));
--     Put_Line ("No_Mixing Gini " & Float'Image (Gini (No_Mixing)));
--     New_Line;
--
--     Some_Mixing.Append ((Num_Features, Green, 3, Apple));
--     Some_Mixing.Append ((Num_Features, Green, 3, Orange));
--     Put_Line ("Some_Mixing Gini " & Float'Image (Gini (Some_Mixing)));
--
--     Lots_Of_Mixing.Append ((Num_Features, Green, 3, Apple));
--     Lots_Of_Mixing.Append ((Num_Features, Green, 3, Orange));
--     Lots_Of_Mixing.Append ((Num_Features, Green, 3, Grape));
--     Lots_Of_Mixing.Append ((Num_Features, Green, 3, Grapefruit));
--     Lots_Of_Mixing.Append ((Num_Features, Green, 3, Blueberry));
--     Put_Line ("Lots_Of_Mixing Gini " & Float'Image (Gini (Lots_Of_Mixing)));
--
--     Current_Uncertainty := Gini (To_Vector (Training_Data));
--     Put_Line ("Current_Uncertainty of Training_Data " &
--                 Float'Image (Current_Uncertainty));
--
--     Question := (Colour_Feature, Green);
--     P_Rows := Partition (To_Vector (Training_Data), Question);
--     Put_Line ("Info gain Green" &
--                 Float'Image (Information_Gain
--                 (P_Rows.True_Rows, P_Rows.False_Rows, Current_Uncertainty)));
--     Print_Rows ("True rows: ", P_Rows.True_Rows);
--     Print_Rows ("False rows: ", P_Rows.False_Rows);
--     New_Line;
--
--     Question := (Colour_Feature, Red);
--     P_Rows := Partition (To_Vector (Training_Data), Question);
--     Put_Line ("Info gain Red" &
--                 Float'Image (Information_Gain
--                 (P_Rows.True_Rows, P_Rows.False_Rows, Current_Uncertainty)));
--     Print_Rows ("True rows: ", P_Rows.True_Rows);
--     Print_Rows ("False rows: ", P_Rows.False_Rows);
--
--     Best := Find_Best_Split (To_Vector (Training_Data));
--     Put_Line ("Best_Split " & Float'Image (Best.Best_Gain) & ", " &
--                 Feature_Type'Image (Best.Best_Question.Feature));
--     if Best.Best_Question.Feature = Colour_Feature then
--        Put_Line ("Best question " & Float'Image (Best.Best_Gain) & ", " &
--                   Colour_Type'Image (Best.Best_Question.Colour_Value));
--     else
--        Put_Line ("Best question is dimension " &
--                    Integer'Image (Best.Best_Question.Diameter_Value));
--     end if;
--
--     aTree := Build_Tree (To_Vector (Training_Data));
--     Print_Tree (aTree);
--
--     Put_Line ("Classify tests, Training_Data (1)");
--     Classified := Classify (Training_Data (1), aTree);
--     Print_Classification (Classified);
--     New_Line;
--
--     Put_Line ("Classify tests, Training_Data (2)");
--     Classified := Classify (Training_Data (2), aTree);
--     Put_Line ("Classify tests,  Classified");
--     Print_Classification (Classified);
--     New_Line;
--     Put_Line ("Classify tests,  Print_Leaf Training_Data (1)");
--     Put_Line (Print_Leaf (Classify (Training_Data (1), aTree)));
--     New_Line;
--     Put_Line ("Classify tests,  Print_Leaf Training_Data (2)");
--     Put_Line (Print_Leaf (Classify (Training_Data (2), aTree)));
--     New_Line;
--     Put_Line ("Evaluate tests");
--     Evaluate (To_Vector (Training_Data), aTree);
end Decision_Tree2;
