--  https://towardsdatascience.com/decision-tree-in-machine-learning-e380942a4c96

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;

with ML_Types; use ML_Types;
with Builder; use Builder;

--  Decision tree models where the target variable can take a discrete set of
--  values are called classification trees.
--  Decision trees where the target variable can take continuous values
--  (typically real numbers) are called regression trees
procedure Decision_Tree is

    function UB (Source : String) return Unbounded_String renames
      To_Unbounded_String;

    Training_Set : constant Data_Rows (1 .. 6) :=
    --  Colour Diameter Label
                     (UB ("Colour,Diameter,Fruit"),
                      UB ("Green, 3, Apple"),
                      UB ("Yellow, 3, Apple"),
                      UB ("Red, 1, Grape"),
                      UB ("Red, 1, Grape"),
                      UB ("Yellow, 3, Lemon"));
    --     No_Mixing : constant Data_Rows (1 .. 3) :=
    --                     --  Colour Diameter Label
    --                       (UB ("Fruit"),
    --                        UB ("Apple"),
    --                        UB ("Apple"));

    aQuestion           : Raw_Question;
    Header              : Header_Data;
    Training_Data       : constant Rows_Vector :=
                            To_Vector (Training_Set, Header);
    Rows                : Partitioned_Rows;
    Mixing_Data         : Row_Data (1);
    Mixing_Feature      : Feature_Names (1 .. 1);
    No_Mixing           : Rows_Vector;
    Some_Mixing         : Rows_Vector;
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
    for index in Header .Features'Range loop
        Put_Line ("Header row features: " &
                    To_String (Header.Features (index)));
    end loop;
    Print_Class_Counts (Training_Data);
    New_Line;

    aQuestion.Feature := (UB ("Colour"));
    aQuestion.Value := (UB ("Green"));
    Print_Question (aQuestion);
    aQuestion.Feature := (UB ("Diameter"));
    aQuestion.Value := (UB ("2"));
    Print_Question (aQuestion);

    New_Line;
    Put_Line ("Partition example");
    aQuestion := (UB ("Colour"), UB ("Red"));
    Print_Question (aQuestion);

    Rows := Partition (Training_Data, (To_Question (aQuestion)));
    Print_Rows ("True rows: ", Rows.True_Rows);
    Print_Rows ("False rows: ", Rows.False_Rows);

    New_Line;
    Put_Line ("Partition example");
    aQuestion := (UB ("Diameter"), UB ("3"));
    Print_Question (aQuestion);

    Rows := Partition (Training_Data, (To_Question (aQuestion)));
    Print_Rows ("True rows: ", Rows.True_Rows);
    Print_Rows ("False rows: ", Rows.False_Rows);

    Mixing_Feature (1) := UB ("Fruit");
    Mixing_Data.Features := Mixing_Feature;
    Mixing_Data.Label := UB ("Apple");
    No_Mixing.Append (Mixing_Data);
    No_Mixing.Append (Mixing_Data);

    Put_Line ("No_Mixing Gini " & Float'Image (Gini (No_Mixing)));
    New_Line;

    Some_Mixing.Clear;
    Some_Mixing.Append (Mixing_Data);
    Mixing_Data.Label := UB ("Orange");
    Some_Mixing.Append (Mixing_Data);
    Print_Rows ("Some_Mixing", Some_Mixing);

    Put_Line ("Some_Mixing Gini " & Float'Image (Gini (Some_Mixing)));
    New_Line;

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
end Decision_Tree;
