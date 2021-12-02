
with Ada.Assertions; use Ada.Assertions;
--  with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;

with Base_Decision_Tree;
with Classifier_Types;
with Classifier_Utilities;
with Decision_Tree_Classification;
--  with Graphviz_Exporter;
with ML_Types;
with Printing;
with Tree;
with Weights;

package body Classifier_Tests is
   use ML_Types;

   --  -------------------------------------------------------------------------

   procedure Test_Diabetes  is
      use Classifier_Utilities;
      use Decision_Tree_Classification;
      use Printing;
      use Classifier_Types.Float_Package;
      Routine_Name    : constant String :=
                          "Test_Diabetes.Test_Probability";
      Diabetes_Data   : constant Data_Record := Load_Data ("src/diabetes.csv");
      theClassifier   : Base_Decision_Tree.Classifier
        (Tree.Integer_Type, Tree.Integer_Type, Tree.Integer_Type);
      X               :  Value_Data_Lists_2D;
      --  Y: num outputs x num classes
      Y               : Value_Data_Lists_2D;
      No_Weights      : Weights.Weight_List := Empty_Vector;
      Num_Samples     : Natural;
      Probabilities   : Weights.Weight_Lists_3D;
      Column_Sums     : Weights.Weight_List;
   begin
      X := Diabetes_Data.Feature_Values;
      Num_Samples := Natural (X.Length);
      Put_Line (Routine_Name);
      Assert (Num_Samples > 0, Routine_Name & " called with empty X vector.");

      --  Y is 2D list num outputs x num classes
      Y := To_Value_2D_List (Diabetes_Data.Label_Values);
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

   end Test_Diabetes;

   --  -------------------------------------------------------------------------

end Classifier_Tests;
