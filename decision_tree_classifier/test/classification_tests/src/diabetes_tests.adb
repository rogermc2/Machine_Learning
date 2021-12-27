
with Ada.Assertions; use Ada.Assertions;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;

with Base_Decision_Tree;
with Classifier_Types;
with Classifier_Utilities;
with Classification_Metrics;
with Criterion;
with Decision_Tree_Classification;
with Graphviz_Exporter;
with ML_Types;
with Printing;
with Tree;
with Weights;

package body Diabetes_Tests is
   use Classifier_Types;
   use ML_Types;

   --  -------------------------------------------------------------------------

   procedure Test_Diabetes_Overfit is
      use Classifier_Utilities;
      use Decision_Tree_Classification;
      --        use Printing;
      use Classifier_Types.Float_Package;
      Routine_Name    : constant String := "Diabetes_Tests.Test_Diabetes_Overfit";
      Diabetes_Data   : constant Multi_Output_Data_Record :=
                         Load_Data ("src/diabetes2.csv");
      theClassifier   : Base_Decision_Tree.Classifier
        (Tree.Float_Type, Tree.Float_Type, Tree.Float_Type);
      Exporter        : Graphviz_Exporter.DOT_Tree_Exporter;
      X               : constant Value_Data_Lists_2D :=
                          Diabetes_Data.Feature_Values;
      Num_Samples     : constant Natural := Natural (X.Length);
      --  Diabetes_Target (Y) : num outputs x num samples
      Diabetes_Target : Value_Data_Lists_2D;
      No_Weights      : Weights.Weight_List := Empty_Vector;
      Prediction      : ML_Types.Value_Data_Lists_2D;
      Score           : Float;
      Success         : Boolean := False;
   begin
      Put_Line (Routine_Name);
      C_Init (theClassifier, "2", Criterion.Gini_Criteria);
      --  L1689
      Assert (Num_Samples > 0, Routine_Name & " called with empty X vector.");

      --  Iris_Target is 2D list num outputs x num samples
      Diabetes_Target := Diabetes_Data.Label_Values;
      Assert (Positive (Diabetes_Target.Length) = Num_Samples, Routine_Name &
                " invalid Diabetes_Target vector");
      --  L309
      Classification_Fit (theClassifier, X, Diabetes_Target, No_Weights);
      Printing.Print_Tree ("The Tree", theClassifier);
      Put_Line ("----------------------------------------------");
      New_Line;

      --  L310
      Prediction := Base_Decision_Tree.Predict (theClassifier, X);
      Printing.Print_Value_Data_Lists_2D
        (Routine_Name & " Predictions", Transpose (Prediction));
      Score := Classification_Metrics.Accuracy_Score
        (Diabetes_Target, Prediction);
      Put_Line (Routine_Name & " Score" &  Float'Image (Score));

      Success := Score > 0.9;
      if Success then
         Put_Line
           ("Classification_Tests Iris prediction test passed.");
      else
         Put_Line
           ("Classification_Tests Iris prediction test failed.");
         Put_Line (Routine_Name & " Score" &  Float'Image (Score));
         New_Line;
      end if;

      Graphviz_Exporter.C_Init
        (Exporter, theClassifier.Attributes.Decision_Tree);
      Graphviz_Exporter.Export_Graphviz
        (Exporter, theClassifier.Attributes.Decision_Tree,
         Output_File_Name => To_Unbounded_String ("diabetes.dot"));

   end Test_Diabetes_Overfit;

   --  -------------------------------------------------------------------------

end Diabetes_Tests;
