
with Ada.Assertions; use Ada.Assertions;
with Ada.Containers;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;

with Base_Decision_Tree;
with Classifier_Types;
with Classifier_Utilities;
with Classification_Metrics;
with Criterion;
with Decision_Tree_Classification;
with Graphviz_Exporter;
with Label;
with ML_Types;
with Printing;
with Tree;
with Utilities;
with Weights;

package body Classifier_Tests is
    use Classifier_Types;
    use ML_Types;

    --     type Clf_Criterions is (Gini, Entropy);
    --     type Reg_Criterions is (Squared_Error, Absolute_Error,
    --                             Friedman_Mse, Poisson);
    --     type Clf_Trees is (Decision_Tree_Classifier, Extra_Tree_Classifier);
    --     type Reg_Trees is (Decision_Tree_Regressor, Extra_Tree_Regressor);
    --  X_Array 6 rows (samples) x 1 column (outputs)
    Min_Split   : constant String := "2";
    X_Array     : constant Multi_Value_Array (1 .. 6, 1 .. 2) :=
                    ((-2, -1), (-1, -1), (-1, -2), (1, 1), (1, 2), (2, 1));
    --  Y_Array 6 rows (samples) x 2 columns (features)
    Y_Array     : constant Integer_Array (1 .. 6) := (-1, -1, -1, 1, 1, 1);
    --  T_Array 3 rows (samples) x 2 columns (features)
    T_Array     : constant Multi_Value_Array (1 .. 3, 1 .. 2) :=
                    ((-1, -1), (2, 2), (3, 2));
    True_Result : constant Integer_Array (1 .. 3) := (-1, 1, 1);

    --  -------------------------------------------------------------------------

    procedure Test_Classification_Toy  is
        use Classifier_Utilities;
        use Decision_Tree_Classification;
        use Printing;
        use Float_Package;
        use Value_Lists_Data_Package;
        Routine_Name      : constant String :=
                              "Classification_Tests.Test_Classification_Toy ";
        Expected          : Value_Data_Lists_2D;
        Prediction        : ML_Types.Value_Data_Lists_2D;
        theClassifier     : Base_Decision_Tree.Classifier
          (Tree.Integer_Type, Tree.Integer_Type, Tree.Integer_Type);
        X                 : constant Value_Data_Lists_2D :=
                              To_Multi_Value_List (X_Array);
        Y                 : Value_Data_Lists_2D;
        T                 : constant Value_Data_Lists_2D :=
                              To_Multi_Value_List (T_Array);
        Num_Samples       : constant Natural := Natural (X.Length);
        No_Weights        : Weights.Weight_List := Float_Package.Empty_Vector;
        Success           : Boolean;
    begin
        C_Init (theClassifier, Min_Split, Criterion.Gini_Criteria);
        Put_Line (Routine_Name);
        Assert (Num_Samples > 0,
                Routine_Name & " called with empty X vector.");

        --  Y is 2D list num outputs x num classes
        Y := To_Integer_Value_List_2D (Y_Array);
        --  L229
        Expected := Transpose (To_Integer_Value_List_2D (True_Result));
        --  L230
        Classification_Fit (theClassifier, X, Y, No_Weights);
        Printing.Print_Tree ("The Tree", theClassifier);
        Put_Line ("----------------------------------------------");
        New_Line;
        Prediction := Base_Decision_Tree.Predict (theClassifier, T);
        Print_Value_Data_Lists_2D
          (Routine_Name & " Predictions", Prediction);
        Print_Value_Data_Lists_2D (Routine_Name & " Expected", Expected);
        New_Line;
        Success := Prediction = Expected;
        if Success then
            Put_Line
              ("Classification_Tests Toy prediction test passed");
        else
            Put_Line
              ("Classification_Tests Toy prediction test failed");
        end if;

        C_Init (theClassifier, Min_Split, Criterion.Gini_Criteria,
                Max_Features => 1);
        Classification_Fit (theClassifier, X, Y, No_Weights);
        Print_Tree ("Max_Features = 1 Tree", theClassifier);
        Put_Line ("----------------------------------------------");
        New_Line;
        Prediction := Base_Decision_Tree.Predict (theClassifier, T);
        Print_Value_Data_Lists_2D
          (Routine_Name & " Predictions", Prediction);
        Print_Value_Data_Lists_2D (Routine_Name & " Expected", Expected);
        New_Line;
        Success := Prediction = Expected;
        if Success then
            Put_Line
              ("Classification Toy Tests Max Features = 1 prediction test passed");
        else
            Put_Line
              ("Classification Toy Tests Max Features = 1 prediction test failed");
        end if;

    end Test_Classification_Toy;

    --  -------------------------------------------------------------------------

    procedure Test_Iris is
        use Ada.Containers;
        use Classifier_Utilities;
        use Decision_Tree_Classification;
        --        use Printing;
        use Classifier_Types.Float_Package;
        Routine_Name   : constant String := "Classifier_Tests.Test_Iris";
        Iris_Data      : constant Multi_Output_Data_Record :=
                           Load_Data ("src/iris.csv");
        theClassifier  : Base_Decision_Tree.Classifier
          (Tree.Float_Type, Tree.Float_Type, Tree.Float_Type);
        Exporter       : Graphviz_Exporter.DOT_Tree_Exporter;
        X              : constant Value_Data_Lists_2D := Iris_Data.Feature_Values;
        Num_Samples    : constant Natural := Natural (X.Length);
        --  Iris_Target (Y) : num outputs x num samples
        Iris_Target    : Value_Data_Lists_2D;
        No_Weights     : Weights.Weight_List := Empty_Vector;
        Prediction     : ML_Types.Value_Data_Lists_2D;
        Score          : Float;
    begin
        Put_Line (Routine_Name);
        C_Init (theClassifier, Min_Split, Criterion.Gini_Criteria);
        --  L1689
        Put_Line (Routine_Name & ", Num_Samples" & Integer'Image (Num_Samples));
        Assert (Num_Samples > 0, Routine_Name & " called with empty X vector.");

        Put_Line (Routine_Name & ", X size: " & Count_Type'Image (X.Length) &
                    " x " & Count_Type'Image (X.Element (1).Length));
        --  Iris_Target is 2D list num outputs x num samples
        Iris_Target := Iris_Data.Label_Values;
        --        Printing.Print_Value_Data_Lists_2D (Routine_Name & ", Iris_Target", Iris_Target);
        Assert (Positive (Iris_Target.Length) = Num_Samples, Routine_Name &
                  " invalid Iris_Target vector");
        --  L1695
        Classification_Fit (theClassifier, X, Iris_Target, No_Weights);
        Put_Line
          (Routine_Name & " Node_Count: " & Count_Type'Image
             (theClassifier.Attributes.Decision_Tree.Nodes.Node_Count - 1));
        --        Printing.Print_Tree ("The Tree", theClassifier);
        Put_Line ("----------------------------------------------");
        New_Line;

        --  L306
        Prediction := Base_Decision_Tree.Predict (theClassifier, X);
        Put_Line
          (Routine_Name & " Num_Samples, Num Predictions: " & Integer'Image
             (Num_Samples) &
             ", " & Integer'Image (Integer (Prediction.Element (1).Length)));
        Printing.Print_Value_Data_Lists_2D
          (Routine_Name & " Predictions", Prediction);
        Put_Line (Routine_Name & ", Iris_Target size: " &
                    Count_Type'Image (Iris_Target.Length) & " x " &
                    Count_Type'Image (Iris_Target.Element (1).Length));
        Put_Line (Routine_Name & ", Transpose (Prediction) size: " &
                    Count_Type'Image (Transpose (Prediction).Length) & " x " &
                    Count_Type'Image (Transpose (Prediction).Element (1).Length));
        Score := Classification_Metrics.Accuracy_Score
          (Iris_Target, Transpose (Prediction));
        Put_Line (Routine_Name & " Score" &  Float'Image (Score));

        Graphviz_Exporter.C_Init
          (Exporter, theClassifier.Attributes.Decision_Tree);
        Graphviz_Exporter.Export_Graphviz
          (Exporter, theClassifier.Attributes.Decision_Tree,
           Output_File_Name => To_Unbounded_String ("iris.dot"));

    end Test_Iris;

    --  -------------------------------------------------------------------------

    procedure Test_Probability  is
        use Classifier_Utilities;
        use Decision_Tree_Classification;
        use Label;
        use Printing;
        use Float_Package;
        use Natural_Package;
        Routine_Name      : constant String :=
                              "Classification_Tests.Test_Probability ";
        Iris_Data         : constant Multi_Output_Data_Record :=
                              Load_Data ("src/iris.csv");
        theClassifier     : Base_Decision_Tree.Classifier
          (Tree.Float_Type, Tree.Float_Type, Tree.Float_Type);
        Exporter          : Graphviz_Exporter.DOT_Tree_Exporter;
        X                 : constant Value_Data_Lists_2D :=
                              To_Multi_Value_List (X_Array);
        Y                 : constant Value_Data_Lists_2D :=
                              To_Integer_Value_List_2D (Y_Array);
        LE_U              : Label_Encoder (Class_Unique);
        Labels            : Classifier_Types.Natural_List;
        Label             : Value_Record (Integer_Type);
        Labels_1D         : Value_Data_List;
        Labels_2D         : Value_Data_Lists_2D;
        X_Iris            : Value_Data_Lists_2D;
        --  Y: num outputs x num classes
        Y_Iris            : Value_Data_Lists_2D;
        No_Weights        : Weights.Weight_List :=
                              Float_Package.Empty_Vector;
        Num_Samples       : Natural;
        Prediction        : ML_Types.Value_Data_Lists_2D;
        Column_Sums       : Weights.Weight_List;
        Prob_Prediction   : Weights.Weight_Lists_3D;
        Max_Arg           : Classifier_Types.Natural_List;
    begin
        Put_Line ("Classification_Tests Toy Probabilities test");
        --  L357
        C_Init (theClassifier, Min_Split, Criterion.Gini_Criteria,
                Max_Depth => 1, Max_Features => 1);

        Num_Samples := Natural (X.Length);
        Put_Line (Routine_Name);
        Assert (Num_Samples > 0, Routine_Name & " called with empty X vector.");

        --  Y is 2D list num outputs x num classes
        Assert (Integer (Y.Length) = Num_Samples, Routine_Name &
                  " invalid Y vector");
        --  L359
        Classification_Fit (theClassifier, X, Y, No_Weights);
        Print_Tree ("Toy Tree", theClassifier);
        Put_Line ("----------------------------------------------");
        New_Line;

        --  Python proba : ndarray of shape (n_samples, n_classes) or
        --  list of n_outputs \ such arrays if n_outputs > 1
        Prob_Prediction := Predict_Probability (theClassifier, X);
        Column_Sums := Classifier_Utilities.Sum_Cols (Prob_Prediction);

        if Column_Sums = Ones (Integer (X.Length)) then
            Put_Line
              ("Classification_Tests Probabilities Column_Sums test passed");
        else
            Put_Line
              ("Classification_Tests Probabilities Column_Sums test failed");
            Print_Weights
              (Routine_Name & " Column_Sums", Column_Sums);
        end if;

        Prediction := Base_Decision_Tree.Predict (theClassifier, X);
        Print_Value_Data_Lists_2D
          ("Classification_Tests Probabilities Prediction: ",
           Transpose (Prediction));
        New_Line;

        --  Iris probabilty test
        X_Iris := Iris_Data.Feature_Values;
        Num_Samples := Natural (X_Iris.Length);
        Put_Line (Routine_Name & "Iris probabilty test");
        Assert (Num_Samples > 0, Routine_Name &
                  " called with empty X_Iris vector.");

        --  Y_Iris is 2D list num outputs x num classes
        Y_Iris := Iris_Data.Label_Values;
        Assert (Integer (Y_Iris.Length) = Num_Samples, Routine_Name &
                  " invalid Y_Iris vector");
        --        Print_Value_Data_List (Routine_Name & "Y_Iris",
        --                               Transpose (Y_Iris).Element (1));
      X_Iris := Utilities.Permute (X_Iris).First_Element;
      Y_Iris := Utilities.Permute (Y_Iris).First_Element;
      Printing.Print_Value_Data_Lists_2D (Routine_Name & "X_Iris",
                                          Transpose (X_Iris));

      Labels := Fit_Transform (LE_U, Transpose (Y_Iris).Element (1));
      Printing.Print_Natural_List (Routine_Name & "Labels", Labels);
        for index in Labels.First_Index .. Labels.Last_Index loop
            Labels_1D.Clear;
            Label.Integer_Value := Labels (index);
            Labels_1D.Append (Label);
            Labels_2D.Append (Labels_1D);
        end loop;
--          Print_Value_Data_Lists_2D (Routine_Name & "Labels_2D",
--                                     Transpose (Labels_2D));
        --  L362
        Classification_Fit (theClassifier, X_Iris, Labels_2D, No_Weights);
        Print_Tree ("The Tree", theClassifier);
        Put_Line ("----------------------------------------------");
        New_Line;

        --  Python proba : ndarray of shape (n_samples, n_classes) or
        --  list of n_outputs \ such arrays if n_outputs > 1
        Prob_Prediction := Predict_Probability (theClassifier, X_Iris);
        Column_Sums := Classifier_Utilities.Sum_Cols (Prob_Prediction);

        Prediction := Base_Decision_Tree.Predict (theClassifier, X_Iris);
        Print_Value_Data_Lists_2D
          ("Classification_Tests Iris Probabilities Prediction: ",
           Transpose (Prediction));
        if Column_Sums = Ones (Integer (X_Iris.Length)) then
            Put_Line
              ("Classification_Tests Probabilities Iris Column_Sums test passed");
        else
            Put_Line
              ("Classification_Tests Probabilities Iris Column_Sums test failed");
            Print_Weights
              (Routine_Name & " Iris Column_Sums", Column_Sums);
        end if;

        Max_Arg := Arg_Max (Prob_Prediction.Element (1));
        Print_Natural_List ("Classification_Tests Iris Probabilities Max_Arg: ",
                            Max_Arg);
        if Max_Arg = To_Natural_List (Prediction.Element (1)) then
            Put_Line
              ("Classification_Tests Iris Probabilities Max_Arg test passed");
        else
            Put_Line
              ("Classification_Tests Iris Probabilities Max_Arg test failed");
            Print_Natural_List ("Classification_Tests Iris Probabilities Max_Arg: ",
                                Max_Arg);
        end if;

        Graphviz_Exporter.C_Init
          (Exporter, theClassifier.Attributes.Decision_Tree);
        Graphviz_Exporter.Export_Graphviz
          (Exporter, theClassifier.Attributes.Decision_Tree,
           Output_File_Name => To_Unbounded_String ("prob_iris.dot"));

    end Test_Probability;

    --  -------------------------------------------------------------------------

    procedure Test_Weighted_Classification_Toy  is
        use Classifier_Utilities;
        use Decision_Tree_Classification;
        use Printing;
        use Float_Package;
        use Value_Lists_Data_Package;
        Routine_Name      : constant String
          := "Classification_Tests.Test_Weighted_Classification_Toy ";
        Exporter          : Graphviz_Exporter.DOT_Tree_Exporter;
        Expected          : Value_Data_Lists_2D;
        Prediction        : ML_Types.Value_Data_Lists_2D;
        theClassifier     : Base_Decision_Tree.Classifier
          (Tree.Integer_Type, Tree.Integer_Type, Tree.Integer_Type);
        X                 : constant Value_Data_Lists_2D :=
                              To_Multi_Value_List (X_Array);
        Y                 : Value_Data_Lists_2D;
        T                 : constant Value_Data_Lists_2D :=
                              To_Multi_Value_List (T_Array);
        Num_Samples       : constant Natural := Natural (X.Length);
        Sample_Weights_1  : Weights.Weight_List := Ones (Num_Samples);
        Sample_Weights_2  : Weights.Weight_List := Set_Value (Num_Samples, 0.5);
        Success           : Boolean;
    begin
        C_Init (theClassifier, Min_Split, Criterion.Gini_Criteria);
        Put_Line (Routine_Name);
        Assert (Num_Samples > 0,
                Routine_Name & " called with empty X vector.");

        --  Y is 2D list num outputs x num classes
        Y := To_Integer_Value_List_2D (Y_Array);
        Expected := Transpose (To_Integer_Value_List_2D (True_Result));

        Put_Line ("Test Weighted Classification Toy 1");
        Classification_Fit (theClassifier, X, Y, Sample_Weights_1);

        Printing.Print_Tree ("Weighted Classification Tree", theClassifier);
        Put_Line ("----------------------------------------------");
        New_Line;

        Graphviz_Exporter.C_Init
          (Exporter, theClassifier.Attributes.Decision_Tree);
        Graphviz_Exporter.Export_Graphviz
          (Exporter, theClassifier.Attributes.Decision_Tree,
           Output_File_Name => To_Unbounded_String ("weighted_1.dot"));

        Prediction := Base_Decision_Tree.Predict (theClassifier, T);
        Print_Value_Data_Lists_2D
          (Routine_Name & " 1.0 weighted Predictions", Prediction);
        Print_Value_Data_Lists_2D
          (Routine_Name & " Expected 1.0 weighted Predictions", Expected);

        Success := Prediction = Expected;

        New_Line;
        Put_Line ("Test Weighted Classification Toy 0.5");
        Classification_Fit (theClassifier, X, Y, Sample_Weights_2);
        Prediction := Base_Decision_Tree.Predict (theClassifier, T);
        Print_Value_Data_Lists_2D
          (Routine_Name & " 0.5 weighted predictions", Prediction);
        Print_Value_Data_Lists_2D
          (Routine_Name & " Expected 0.5 weighted predictions", Expected);

        Success := Success and Prediction = Expected;
        if Success then
            Put_Line
              ("Weighted Classification Tests Toy prediction test passed");
        else
            Put_Line
              ("Weighted Classification Tests Toy prediction test failed");
        end if;

    end Test_Weighted_Classification_Toy;

    --  -------------------------------------------------------------------------

end Classifier_Tests;
