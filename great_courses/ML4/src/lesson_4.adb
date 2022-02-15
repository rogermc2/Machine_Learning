
with Ada.Assertions; use Ada.Assertions;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;

with GNATCOLL.JSON; use GNATCOLL.JSON;

with ML_Types;

--  with Base_Decision_Tree;
--  with Classifier_Types;
--  with Classifier_Utilities;
--  with Criterion;
with Data_Splitter;
--  with Decision_Tree_Classification;
--  with Graphviz_Exporter;
with Openml;
--  with Printing;
--  with Tree;
with Utilities;
--  with Weights;

procedure Lesson_4 is
   use ML_Types;
   use String_Package;
--     use Decision_Tree_Classification;
--     use Printing;
   Routine_Name  : constant String := "Lesson_4 ";
--     Min_Split     : constant String := "2";
--     Data          : constant Multi_Output_Data_Record :=
--                       Classifier_Utilities.Load_Data ("src/diabetes.csv");
--     Feature_Names : constant String_List := Data.Feature_Names;
   Train_Samples : constant Positive := 5000;
   Test_Size     : constant Positive := 1500;
   Train_Size    : constant Positive := Train_Samples - Test_Size;
   Data_Id       : Integer := 0;
   As_Frame      : Unbounded_String := To_Unbounded_String ("false");
   XY            : Openml.Bunch_Data (True);
   X             : String_List;
   Y             : String_List;
   Num_Samples   : Positive;
   Test_Data     : String_Vector;
   Train_Data    : String_Vector;
--     Names_Cursor  : String_Package.Cursor := Feature_Names.First;
--     Features      : Feature_Names_List;
--     aClassifier   : Base_Decision_Tree.Classifier
--       (Tree.Integer_Type, Tree.Integer_Type, Tree.Integer_Type);
--     Labels        : Value_Data_Lists_2D;
--     No_Weights    : Weights.Weight_List :=
--                       Classifier_Types.Float_Package.Empty_Vector;
--     Correct       : Natural := 0;
--     Exporter      : Graphviz_Exporter.DOT_Tree_Exporter;
begin
   Put_Line (Routine_Name);
   XY := Openml.Fetch_Openml (Dataset_Name => "mnist_784", Version => "",
                              Use_Files    => True, Data_Id => Data_Id,
                              Target_Column => Empty_List, Return_X_Y => True,
                              As_Frame  => As_Frame);

   Put_Line (Routine_Name & "X length" & Integer'Image (Length (XY.Data)));
   Put_Line (Routine_Name & "Y length" & Integer'Image (Length (XY.Target)));
   Assert (Length (XY.Data) > 0, Routine_Name & "X is empty.");
   Assert (Length (XY.Target) > 0, Routine_Name & "Y is empty.");

   X := Openml.J_Array_To_String_List (XY.Data);
   Y := Openml.J_Array_To_String_List (XY.Target);
   Num_Samples := Positive (Length (X));

   Assert (Natural (Length (Y)) = Num_Samples, Routine_Name &
             "Y length" & Integer'Image (Integer (Length (Y))) &
             " is different to X length" & Natural'Image (Num_Samples));

   Utilities.Permute (X);
   Utilities.Permute (Y);

   Data_Splitter.Train_Test_Split (X, Y, Test_Size, Train_Size, Test_Data,
                                   Train_Data);

--     Assert (Num_Samples > 0, Routine_Name & " called with empty X vector.");
--     --  Labels is 2D list num outputs x num samples
--     Labels := Data.Label_Values;
--     while Has_Element (Names_Cursor) loop
--        Features.Append (Element (Names_Cursor));
--        Next (Names_Cursor);
--     end loop;
--     Print_Unbounded_List ("Features", Features);
--     Print_Value_Data_List ("Features row 16", X_Data.Element (16));
--     New_Line;
--
--     C_Init (aClassifier, Min_Split, Criterion.Gini_Criteria,
--             Max_Leaf_Nodes => 6);
--
--     --  Fit function adjusts weights according to data values so that
--     --  better accuracy can be achieved
--     Classification_Fit (aClassifier, X_Data, Labels, No_Weights);
--     Printing.Print_Tree ("Diabetes Tree", aClassifier);
--     Put_Line ("----------------------------------------------");
--     New_Line;
--
--     for index in X_Data.First_Index .. X_Data.Last_Index loop
--          if Base_Decision_Tree.Predict
--            (aClassifier, X_Data).Element (index).Element (1) =
--                Labels.Element (index).Element (1) then
--             Correct := Correct + 1;
--          end if;
--     end loop;
--     Put_Line ("Prediction: " &
--                 Float'Image (100.0 * Float (Correct) / Float (X_Data.Length)));
--     New_Line;
--
--     Graphviz_Exporter.C_Init
--       (Exporter, aClassifier.Attributes.Decision_Tree);
--     Graphviz_Exporter.Export_Graphviz
--       (Exporter, aClassifier.Attributes.Decision_Tree, Feature_Names => Features,
--        Output_File_Name => To_Unbounded_String ("diabetes.dot"));

end Lesson_4;
