
with Ada.Assertions; use Ada.Assertions;
with Ada.Containers;
with Ada.Text_IO; use Ada.Text_IO;

with Utilities;

with Base_Decision_Tree;
with Classifier_Types;
with Classifier_Utilities;
with Criterion;
with Decision_Tree_Classification;
with ML_Types;
with Node_Splitter;
with Printing;
with Tree;
with Weights;

package body Decision_Path_Tests is
    use ML_Types;

    --  -------------------------------------------------------------------------

    procedure Test_Decision_Path  is
        use Classifier_Utilities;
        use Decision_Tree_Classification;
        use Printing;
        use Classifier_Types.Float_Package;
        Routine_Name      : constant String :=
                              "Decision_Path_Tests.Test_Decision_Path";
        Iris_Data         : constant Data_Record := Load_Data ("src/iris.csv");
        Criteria          : Criterion.Criterion_Class;
        Splitter          : Node_Splitter.Splitter_Class;
        theClassifier     : Base_Decision_Tree.Classifier
          (Tree.Float_Type, Tree.Float_Type, Tree.Float_Type);
        X                 :  Value_Data_Lists_2D;
        --  Y: num outputs x num classes
        Y                 : Value_Data_Lists_2D;
        No_Weights        : Weights.Weight_List := Empty_Vector;
        Num_Samples       : Natural;
        Probabilities     : Weights.Weight_Lists_3D;
        Column_Sums       : Weights.Weight_List;
        Min_Samples_Split : ML_Types.Value_Record (Integer_Type);
    begin
        Min_Samples_Split.Integer_Value := 2;
        C_Init (theClassifier, Criteria, Splitter, Min_Samples_Split);
        --  L1689
        X := Iris_Data.Feature_Values;
        Num_Samples := Natural (X.Length);
        Put_Line (Routine_Name);
        Assert (Num_Samples > 0, Routine_Name & " called with empty X vector.");

        --  Y is 2D list num outputs x num classes
        Y := To_Value_2D_List (Iris_Data.Label_Values);
        Assert (Integer (Y.Length) = Num_Samples, Routine_Name &
                  " invalid Y vector");
        --  L1695
        Classification_Fit (theClassifier, X, Y, No_Weights);
        Print_Tree ("The Tree", theClassifier);
        Put_Line ("----------------------------------------------");
        New_Line;
        --  L358 test_probability
        Probabilities := Predict_Probability (theClassifier, X);
        Column_Sums := Classifier_Utilities.Sum_Cols (Probabilities);

        if Column_Sums = Ones (Integer (X.Length)) then
            Put_Line ("Probabilities test passed");
        else
            Put_Line ("Probabilities test failed");
            Print_Weights (Routine_Name & " Column_Sums", Column_Sums);
        end if;

    end Test_Decision_Path;

    --  -------------------------------------------------------------------------

    procedure Test_Iris is
        use Ada.Containers;
        use Classifier_Utilities;
        use Decision_Tree_Classification;
        use Printing;
        use Classifier_Types.Float_Package;
        Routine_Name  : constant String := "Decision_Path_Tests.Test_Iris";
        Criteria      : Criterion.Criterion_Class;
        Splitter      : Node_Splitter.Splitter_Class;
        Data_File     : File_Type;
        Iris_CSV_Data : ML_Types.Rows_Vector;
        Iris_Data     : Data_Record;
        theClassifier : Base_Decision_Tree.Classifier
          (Tree.Float_Type, Tree.Float_Type, Tree.Float_Type);
        X             :  Value_Data_Lists_2D;
        --  Y: num outputs x num classes
        Y             : Value_Data_Lists_2D;
        No_Weights    : Weights.Weight_List := Empty_Vector;
        Num_Samples   : Natural;
        Prediction    : ML_Types.Value_Data_Lists_2D;
        Min_Samples_Split : ML_Types.Value_Record (Integer_Type);
    begin
        Min_Samples_Split.Integer_Value := 2;
        C_Init (theClassifier, Criteria, Splitter, Min_Samples_Split);
        Open (Data_File, In_File, "src/iris.csv");
        Utilities.Load_CSV_Data (Data_File, Iris_CSV_Data);
        Close (Data_File);
        Iris_Data := Utilities.Split_Row_Data (Iris_CSV_Data);
        --  L1689
        X := Iris_Data.Feature_Values;
        Num_Samples := Natural (X.Length);
        Put_Line (Routine_Name & ", Num_Samples" & Integer'Image (Num_Samples));
        Put_Line (Routine_Name);
        Assert (Num_Samples > 0, Routine_Name & " called with empty X vector.");

        --  Y is 2D list num outputs x num classes
        Y := To_Value_2D_List (Iris_Data.Label_Values);
        Assert (Integer (Y.Length) = Num_Samples, Routine_Name &
                  " invalid Y vector");
        --  L1695
        Classification_Fit (theClassifier, X, Y, No_Weights);
        Put_Line (Routine_Name & ", Node_Count" & Count_Type'Image
                  (theClassifier.Attributes.Decision_Tree.Nodes.Node_Count - 1));
        Print_Tree ("The Tree", theClassifier);
        Put_Line ("----------------------------------------------");
        New_Line;
        Prediction := Base_Decision_Tree.Predict (theClassifier, X);
        Print_Value_Data_Lists_2D
          (Routine_Name & " Predictions", Prediction);

    end Test_Iris;

    --  -------------------------------------------------------------------------

end Decision_Path_Tests;
