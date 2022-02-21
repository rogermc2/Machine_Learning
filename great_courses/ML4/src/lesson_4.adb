
with Ada.Containers;
with Ada.Assertions; use Ada.Assertions;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;

with ML_Types;

--  with Base_Decision_Tree;
--  with Classifier_Types;
--  with Classifier_Utilities;
--  with Criterion;
with Data_Splitter;
--  with Decision_Tree_Classification;
--  with Graphviz_Exporter;
with Load_ARFF_Data;
with Openml_Ada;
--  with Printing;
--  with Tree;
--  with Weights;

procedure Lesson_4 is
    use Ada.Containers;
    use ML_Types;
    use String_Package;
    use Load_ARFF_Data;
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
    As_Frame      : Openml_Ada.As_Frame_State := Openml_Ada.As_Frame_False;
    Bunch         : Openml_Ada.Bunch_Data;
    X             : ARFF_Data_List_2D;  --  rows of columns of values
    Y             : ARFF_Data_List_2D;
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
    Openml_Ada.Fetch_Openml (Dataset_File_Name => "../iris.arff",
                             Save_File_Name    => "iris.oml",
                             Target_Column     => Empty_List,
                             X                 => X,
                             Y                 => Y,
                             Bunch             => Bunch,
                             As_Frame          => As_Frame,
                             Return_X_Y        => True);

    Put_Line (Routine_Name & "X length" & Count_Type'Image (X.Length));
    Put_Line (Routine_Name & "Y length" & Count_Type'Image (Y.Length));
    Assert (X.Length > 0, Routine_Name & "X is empty.");
    Assert (Y.Length > 0, Routine_Name & "Y is empty.");

    Num_Samples := Positive (X.Length);

    Assert (Natural (Y.Length) = Num_Samples, Routine_Name &
              "Y length" & Count_Type'Image (Y.Length) &
              " is different to X length" & Natural'Image (Num_Samples));

    X := Permute (X);
    Y := Permute (Y);

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
