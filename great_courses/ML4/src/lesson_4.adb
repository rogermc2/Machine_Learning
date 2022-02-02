
with Ada.Assertions; use Ada.Assertions;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;

with GNATCOLL.JSON; use GNATCOLL.JSON;

with ML_Types;

--  with Base_Decision_Tree;
--  with Classifier_Types;
--  with Classifier_Utilities;
--  with Criterion;
--  with Decision_Tree_Classification;
--  with Graphviz_Exporter;
with Openml;
--  with Printing;
--  with Tree;
with Utilities;
--  with Weights;

procedure Lesson_4 is
   use ML_Types;
--     use ML_Types.String_Package;
--     use Decision_Tree_Classification;
--     use Printing;
   Routine_Name  : constant String := "Lesson_4 ";
--     Min_Split     : constant String := "2";
--     Data          : constant Multi_Output_Data_Record :=
--                       Classifier_Utilities.Load_Data ("src/diabetes.csv");
--     Feature_Names : constant String_List := Data.Feature_Names;
--     X_Data        : constant Value_Data_Lists_2D := Data.Feature_Values;
--     Num_Samples   : constant Natural := Natural (X_Data.Length);
   Data_Id       : Integer := 0;
   As_Frame      : Unbounded_String := To_Unbounded_String ("false");
   XY            : Openml.Bunch_Data (True);
   X             : String_List;
   Y             : String_List;
   Train_Samples : constant Positive := 5000;
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
   XY := Openml.Fetch_Openml
     (Dataset_Name  => "mnist_784", Version  => "",
      File_Name => "../mnist_784",
      Features_File_Name => "../dataset_554_features", Data_Id  => Data_Id,
      Target_Column => "default-target", Return_X_Y  => True,
      As_Frame => As_Frame);
   Put_Line (Routine_Name & "X length" & Integer'Image (Length (XY.Data)));
   Put_Line (Routine_Name & "Y length" & Integer'Image (Length (XY.Target)));
   Assert (Length (XY.Data) > 0, Routine_Name & "X is empty.");
   Assert (Length (XY.Target) > 0, Routine_Name & "Y is empty.");

   X := Openml.J_Array_To_String_List (XY.Data);
   Y := Openml.J_Array_To_String_List (XY.Target);
   X := Utilities.Permute (X);

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
