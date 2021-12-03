
with Ada.Assertions; use Ada.Assertions;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;

with Base_Decision_Tree;
with Classifier_Types;
with Classifier_Utilities;
with Criterion;
with Decision_Tree_Classification;
with Graphviz_Exporter;
with ML_Types;
with Node_Splitter;
with Printing;
with Tree;
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
      Routine_Name      : constant String :=
                            "Classification_Tests.Test_Classification_Toy";
      Criteria          : Criterion.Criterion_Class;
      Splitter          : Node_Splitter.Splitter_Class;
      Expected          : Value_Data_Lists_2D;
      Prediction        : ML_Types.Value_Data_Lists_2D;
      theClassifier     : Base_Decision_Tree.Classifier
        (Tree.Integer_Type, Tree.Integer_Type, Tree.Integer_Type);
      Exporter          : Graphviz_Exporter.DOT_Tree_Exporter;
      X                 : constant Value_Data_Lists_2D :=
                            To_Multi_Value_List (X_Array);
      Y                 : Value_Data_Lists_2D;
      T                 : constant Value_Data_Lists_2D :=
                            To_Multi_Value_List (T_Array);
      Num_Samples       : constant Natural := Natural (X.Length);
      No_Weights        : Weights.Weight_List := Empty_Vector;
      Sample_Weights_1  : Weights.Weight_List := Ones (Num_Samples);
      Sample_Weights_2  : Weights.Weight_List := Set_Value (Num_Samples, 0.5);
      Probabilities     : Weights.Weight_Lists_3D :=
                              Weights.Weight_Lists_3D_Package.Empty_Vector;
      Column_Sums       : Weights.Weight_List := Empty_Vector;
   begin
      C_Init (theClassifier, Criteria, Splitter);
      Put_Line (Routine_Name);
      Assert (Num_Samples > 0,
              Routine_Name & " called with empty X vector.");

      --  Y is 2D list num outputs x num classes
      Y := To_Integer_Value_List_2D (Y_Array);
      --  L229
      Expected := To_Integer_Value_List_2D (True_Result);
      --  L230
      Classification_Fit (theClassifier, X, Y, No_Weights);
      Print_Tree ("The Tree", theClassifier);
      Put_Line ("----------------------------------------------");
      New_Line;
      Prediction := Base_Decision_Tree.Predict (theClassifier, T);
      Print_Value_Data_Lists_2D
        (Routine_Name & " Predictions", Prediction);
      Print_Value_Data_Lists_2D (Routine_Name & " Expected", Expected);
      --  L353 test_probability
      Probabilities := Predict_Probability (theClassifier, X);
      Column_Sums := Classifier_Utilities.Sum_Cols (Probabilities);

      if Column_Sums = Ones (Integer (X.Length)) then
         Put_Line ("Classification_Tests Probabilities test passed");
      else
         Put_Line ("Classification_Tests Probabilities test failed");
         Print_Weights
           (Routine_Name & " Column_Sums", Column_Sums);
      end if;

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

      Put_Line ("Test Weighted Classification Toy 0.5");
      Classification_Fit (theClassifier, X, Y, Sample_Weights_2);
      Prediction := Base_Decision_Tree.Predict (theClassifier, T);
      Print_Value_Data_Lists_2D
        (Routine_Name & " 0.5 weighted predictions", Prediction);
      Print_Value_Data_Lists_2D
        (Routine_Name & " Expected 0.5 weighted predictions", Expected);

   end Test_Classification_Toy;

   --  -------------------------------------------------------------------------

   procedure Test_Probability  is
      use Classifier_Utilities;
      use Decision_Tree_Classification;
      use Printing;
      use Float_Package;
      Routine_Name    : constant String :=
                          "Classification_Tests.Test_Probability";
      Iris_Data       : constant Data_Record := Load_Data ("src/iris.csv");
      Criteria        : Criterion.Criterion_Class;
      Splitter        : Node_Splitter.Splitter_Class;
      theClassifier   : Base_Decision_Tree.Classifier
        (Tree.Float_Type, Tree.Float_Type, Tree.Float_Type);
      X               :  Value_Data_Lists_2D;
      --  Y: num outputs x num classes
      Y               : Value_Data_Lists_2D;
      No_Weights      : Weights.Weight_List := Empty_Vector;
      Num_Samples     : Natural;
      Probabilities   : Weights.Weight_Lists_3D;
      Column_Sums     : Weights.Weight_List;
   begin
      C_Init (theClassifier, Criteria, Splitter);
      X := Iris_Data.Feature_Values;
      Num_Samples := Natural (X.Length);
      Put_Line (Routine_Name);
      Assert (Num_Samples > 0, Routine_Name & " called with empty X vector.");

      --  Y is 2D list num outputs x num classes
      Y := To_Value_2D_List (Iris_Data.Label_Values);
      Assert (Integer (Y.Length) = Num_Samples, Routine_Name &
                " invalid Y vector");
      --  L356
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

   end Test_Probability;

   --  -------------------------------------------------------------------------

end Classifier_Tests;
