
with Ada.Assertions; use Ada.Assertions;
with Ada.Text_IO; use Ada.Text_IO;

with Base_Decision_Tree;
with Classifier_Types;
with Classifier_Utilities;
with Decision_Tree_Classification;
with ML_Types;
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
      Expected        : Value_Data_Lists_2D;
      Prediction      : ML_Types.Value_Data_Lists_2D;
      theClassifier   : Base_Decision_Tree.Classifier
        (Tree.Integer_Type, Tree.Integer_Type, Tree.Integer_Type);
      --        Max_Depth       : constant Positive := 5;
      X               : constant Value_Data_Lists_2D :=
                          To_Multi_Value_List (X_Array);
      Y               : Value_Data_Lists_2D;
      T               : constant Value_Data_Lists_2D :=
                          To_Multi_Value_List (T_Array);
      Num_Samples     : constant Natural := Natural (X.Length);
      Probabilities   : Weights.Weight_Lists_3D;
--        Column_Sums     : Value_Data_List;
   begin
      Put_Line ("Classification_Tests.Test_Classification_Toy:");
      Assert (Num_Samples > 0,
              "Classification_Tests.Test_Classification_Toy called with empty X vector.");

      --  Y is 2D list num outputs x num classes
      Y := To_Integer_Value_List_2D (Y_Array);
      --  L229
      Expected := To_Integer_Value_List_2D (True_Result);
      --  L230
      Classification_Fit (theClassifier, X, Y);
      Print_Tree ("The Tree", theClassifier);
      Put_Line ("----------------------------------------------");
      New_Line;
      Prediction := Base_Decision_Tree.Predict (theClassifier, T);
      Print_Value_Data_Lists_2D
        ("Classification_Tests.Test_Classification_Toy Predictions",
         Prediction);
      Print_Value_Data_Lists_2D
        ("Classification_Tests.Test_Classification_Toy Expected", Expected);
      --  L353 test_probability
      Probabilities := Predict_Probability (theClassifier, X);
--        Column_Sums := Classifier_Utilities.Sum_Cols (Probabilities);
      --        if Probabilities = Expected then
      --           null;
      --        end if;
      Print_Weight_Lists_3D
        ("Classification_Tests.Test_Classification_Toy Probabilities",
         Probabilities);

   end Test_Classification_Toy;

   --  -------------------------------------------------------------------------

end Classifier_Tests;
