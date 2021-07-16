--  https://towardsdatascience.com/decision-tree-in-machine-learning-e380942a4c96

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;

with Builder; use Builder;
with ML_Types; use ML_Types;
with Utilities;

--  Decision tree models where the target variable can take a discrete set of
--  values are called classification trees.
--  Decision trees where the target variable can take continuous values
--  (typically real numbers) are called regression trees
procedure Decision_Tree is

   function UB (Source : String) return Unbounded_String renames
     To_Unbounded_String;

   Training_Set : constant Data_Rows (1 .. 6) :=
   --  Feature 1, Feature 2, Label
                    (UB ("Colour,Diameter,Fruit"),
                     UB ("Green, 3, Apple"),
                     UB ("Yellow, 3, Apple"),
                     UB ("Red, 1, Grape"),
                     UB ("Red, 1, Grape"),
                     UB ("Yellow, 3, Lemon"));

   --      Training_Set2 : constant Data_Rows (1 .. 4) :=
   --                       (UB ("Colour,Diameter,Fruit"),
   --                        UB ("Green, 3, Apple"),
   --                        UB ("Yellow, 3, Apple"),
   --                        UB ("Yellow, 3, Lemon"));

   aRaw_Question       : Raw_Question;
   aQuestion           : Question_Data;  --  Default is integer data
   Colour_Feature      : constant Feature_Name_Type :=
                           To_Unbounded_String ("Colour");
   Diameter_Feature    : constant Feature_Name_Type :=
                           To_Unbounded_String ("Diameter");
   Training_Data       : constant Rows_Vector := Initialize (Training_Set);
   --      Training_Data2      : Rows_Vector;
   Header              : constant Header_Data_Type := Header_Row;
   Gain_Zero           : constant Float := 0.0;
   Mixing_Data         : Row_Data (1);
   Mixing_Feature      : Feature_Data_Array (1 .. 1);
   Empty_Rows          : Rows_Vector;
   No_Mixing           : Rows_Vector;
   Some_Mixing         : Rows_Vector;
   Lots_Of_Mixing      : Rows_Vector;
   Current_Uncertainty : Float;
   P_Rows              : Partitioned_Rows;
   Best_Test           : Best_Data;
   aTree               : Tree_Type;
   Classified          : Prediction_Data_List;
begin
   New_Line;
   Put ("Features:");
   for index in Header.Features'Range loop
      Put (" " & To_String (Header.Features (index)));
   end loop;
   New_Line;
   Utilities.Print_Unique_Values (Training_Data, Colour_Feature);
   Utilities.Print_Unique_Values (Training_Data, Diameter_Feature);
   Utilities.Print_UB_Label_Counts (Training_Data);
   New_Line;

   aRaw_Question.Feature_Name := Colour_Feature;
   aRaw_Question.Feature_Value := (UB ("Green"));
   Utilities.Print_Raw_Question ("Raw question example 1", aRaw_Question);
   aRaw_Question.Feature_Name := Diameter_Feature;
   aRaw_Question.Feature_Value := (UB ("2"));
   Utilities.Print_Raw_Question ("Raw question example 2", aRaw_Question);

   New_Line;
   Put_Line ("Partition example 1");
   aRaw_Question := (Colour_Feature, UB ("Red"));
   Utilities.Print_Raw_Question ("Partition raw question", aRaw_Question);

   Put_Line (" Calling Partition 1");
   P_Rows := Partition (Training_Data, (To_Question (aRaw_Question)));
   Utilities.Print_Rows ("True rows: ", P_Rows.True_Rows);
   Utilities.Print_Rows ("False rows: ", P_Rows.False_Rows);

   New_Line;
   Put_Line ("Partition example 2");
   aRaw_Question := (Diameter_Feature, UB ("3"));
   Utilities.Print_Raw_Question ("Partition example raw question", aRaw_Question);
   Put_Line ("Partition aRaw_Question set");

   P_Rows := Partition (Training_Data, (To_Question (aRaw_Question)));
   Utilities.Print_Rows ("True rows: ", P_Rows.True_Rows);
   Utilities.Print_Rows ("False rows: ", P_Rows.False_Rows);

   Mixing_Feature (1) := UB ("Fruit");
   Mixing_Data.Features := Mixing_Feature;
   Mixing_Data.Label := UB ("Apple");
   No_Mixing.Append (Mixing_Data);
   No_Mixing.Append (Mixing_Data);

   New_Line;
   Put_Line ("No_Mixing Gini " & Float'Image (Gini (No_Mixing)));
   New_Line;

   Some_Mixing.Clear;
   Some_Mixing.Append (Mixing_Data);
   Mixing_Data.Label := UB ("Orange");
   Some_Mixing.Append (Mixing_Data);
   Utilities.Print_Rows ("Some_Mixing", Some_Mixing);

   Put_Line ("Some_Mixing Gini " & Float'Image (Gini (Some_Mixing)));
   New_Line;

   Mixing_Data.Features := Mixing_Feature;
   Lots_Of_Mixing.Append ((1, Mixing_Feature, UB ("Apple")));
   Lots_Of_Mixing.Append ((1, Mixing_Feature, UB ("Orange")));
   Lots_Of_Mixing.Append ((1, Mixing_Feature, UB ("Grape")));
   Lots_Of_Mixing.Append ((1, Mixing_Feature, UB ("Grapefruit")));
   Lots_Of_Mixing.Append ((1, Mixing_Feature, UB ("Blueberry")));
   Put_Line ("Lots_Of_Mixing Gini " & Float'Image (Gini (Lots_Of_Mixing)));

   Current_Uncertainty := Gini (Training_Data);
   Put_Line ("Current_Uncertainty (Gini) of Training_Data " &
               Float'Image (Current_Uncertainty));

   Empty_Rows.Clear;
   aQuestion := (Integer_Type, Diameter_Feature, Gain_Zero, 3);
   --                   Empty_Rows, Empty_Rows, Gain_Zero, 3);
   P_Rows := Partition (Training_Data, aQuestion);
   New_Line;
   Put_Line ("Training_Data partitioned");
   Put_Line ("Info gain dimension 3" &
               Float'Image (Information_Gain
               (P_Rows.True_Rows, P_Rows.False_Rows, Current_Uncertainty)));
   Utilities.Print_Rows ("True rows: ", P_Rows.True_Rows);
   Utilities.Print_Rows ("False rows: ", P_Rows.False_Rows);
   New_Line;

   Empty_Rows.Clear;
   aQuestion := (UB_String_Type, Colour_Feature, Gain_Zero, UB ("Red"));
   P_Rows := Partition (Training_Data, aQuestion);
   Put_Line ("Info gain Red" &
               Float'Image (Information_Gain
               (P_Rows.True_Rows, P_Rows.False_Rows, Current_Uncertainty)));
   Utilities.Print_Rows ("True rows: ", P_Rows.True_Rows);
   Utilities.Print_Rows ("False rows: ", P_Rows.False_Rows);

   New_Line;
   Put_Line ("Find_Best_Split");
   Best_Test := Find_Best_Split (Training_Data);
   Put_Line ("Best_Split " & Float'Image (Gain (Best_Test)) & ", " &
               To_String (Best_Question (Best_Test).Feature_Name));
   Utilities.Print_Best ("Decision_Tree", Best_Test);

   --      New_Line;
   --      Put_Line ("-------------------------------------------------------");
   --      Training_Data2 := Initialize (Training_Set2);
   --      Current_Uncertainty := Gini (Training_Data2);
   --      Put_Line ("Current_Uncertainty (Gini) of Training_Data2 " &
   --                  Float'Image (Current_Uncertainty));
   --
   --      New_Line;
   --      Put_Line ("Find_Best_Split 2");
   --      Best_Test := Find_Best_Split (Training_Data2);
   --      Put_Line ("Best_Split 2 " & Float'Image (Gain (Best_Test)) & ", " &
   --                  To_String (Best_Question (Best_Test).Feature_Name));
   --      Utilities.Print_Best ("Decision_Tree 2", Best_Test);
   --
   --      Training_Data := Initialize (Training_Set);

   New_Line;
   Put_Line ("Build Tree");
   aTree := Build_Tree (Training_Data);
   New_Line;

   Put_Line ("Print Tree");
   Utilities.Print_Tree (aTree);
   New_Line;

   Put_Line ("Classify tests, Training_Data (1)");
   Classified := Classify
     (Training_Data (1), Tree_Package.First_Child (aTree.Root));
   Utilities.Print_Classification (Classified);
   New_Line;

   Put_Line ("Classify tests, Training_Data (2)");
   Classified := Classify (Training_Data (2),
                           Tree_Package.First_Child (aTree.Root));
   Put_Line ("Classify tests,  Classified");
   Utilities.Print_Classification (Classified);
   New_Line;
   Put_Line ("Classify tests,  Print_Leaf Training_Data (1)");
   Utilities.Print_Leaf (Classify (Training_Data (1),
                         Tree_Package.First_Child (aTree.Root)));
   New_Line;
      Put_Line ("Classify tests,  Print_Leaf Training_Data (2)");
      Utilities.Print_Leaf (Classify (Training_Data (2),
                            Tree_Package.First_Child (aTree.Root)));
      New_Line;
   --     Put_Line ("Evaluate tests");
   --     Evaluate (To_Vector (Training_Data), aTree);
end Decision_Tree;
