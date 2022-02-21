
with Ada.Containers;
with Ada.Assertions; use Ada.Assertions;
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
with Load_ARFF_Data.ARFF_Printing;
with Openml_Ada;
with Printing;
--  with Tree;
--  with Weights;

procedure Lesson_4 is
    use Ada.Containers;
    use ML_Types;
    use String_Package;
    use Load_ARFF_Data;
    --     use Decision_Tree_Classification;
    Routine_Name  : constant String := "Lesson_4 ";
    --     Min_Split     : constant String := "2";
    As_Frame      : Openml_Ada.As_Frame_State := Openml_Ada.As_Frame_False;
    Bunch         : Openml_Ada.Bunch_Data;
    X             : ARFF_Data_List_2D;  --  rows of columns of values
    Y             : ARFF_Data_List_2D;
    Num_Samples   : Positive;
    Test_Size     : Positive;
    Train_Size    : Positive;
    Test_Data     : ARFF_Data_List;
    Train_Data    : ARFF_Data_List;
    --     aClassifier   : Base_Decision_Tree.Classifier
    --       (Tree.Integer_Type, Tree.Integer_Type, Tree.Integer_Type);
    --     No_Weights    : Weights.Weight_List :=
    --                       Classifier_Types.Float_Package.Empty_Vector;
    --     Correct       : Natural := 0;
    --     Exporter      : Graphviz_Exporter.DOT_Tree_Exporter;
begin
    Put_Line (Routine_Name);
    Openml_Ada.Fetch_Openml (Dataset_File_Name => "../diabetes.arff",
                             Save_File_Name    => "diabetes.oml",
                             Target_Column     => Empty_List,
                             X                 => X,
                             Y                 => Y,
                             Bunch             => Bunch,
                             As_Frame          => As_Frame,
                             Return_X_Y        => False);
    Num_Samples := Positive (X.Length);
    Test_Size := Num_Samples / 4;
    Train_Size := Num_Samples - Test_Size;

    Put_Line (Routine_Name & "Num_Samples" & Integer'Image (Num_Samples));
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
--     ARFF_Printing.Print_Data (Routine_Name & "X", X, 1, 4);
--     ARFF_Printing.Print_Data (Routine_Name & "Train_Data", Train_Data);

    Printing.Print_Strings ("Features", Bunch.Feature_Names);
    ARFF_Printing.Print_Data ("Features row 16", X.Element (16));
    New_Line;

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
