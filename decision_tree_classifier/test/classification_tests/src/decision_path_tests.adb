--  Based on scikit-learn/sklearn/tree/tests check_decision_path L1718

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
--  with Printing;
with Tree;
with Weights;

package body Decision_Path_Tests is
   use ML_Types;

   --  -------------------------------------------------------------------------
   --  Based on test_decision_path L1718 which calls check_decision_path L1688
   procedure Test_Decision_Path  is
      use Classifier_Utilities;
      use Decision_Tree_Classification;
--        use Printing;
      use Classifier_Types.Float_Package;
      Routine_Name      : constant String :=
                            "Decision_Path_Tests.Test_Decision_Path";
      Exporter          : Graphviz_Exporter.DOT_Tree_Exporter;
      Iris_Data         : constant Data_Record := Load_Data ("src/iris.csv");
      X                 :  constant Value_Data_Lists_2D :=
                            Iris_Data.Feature_Values;
      theClassifier     : Base_Decision_Tree.Classifier
        (Tree.Float_Type, Tree.Float_Type, Tree.Float_Type);
      --  Y: num outputs x num classes
      Y                 : Value_Data_Lists_2D;
      No_Weights        : Weights.Weight_List := Empty_Vector;
      Num_Samples       : Natural;
   begin
      C_Init (theClassifier, "2", Criterion.Gini_Criteria);
      --  L1689
      Num_Samples := Natural (X.Length);
      Put_Line (Routine_Name);
      Assert (Num_Samples > 0, Routine_Name & " called with empty X vector.");

      --  Y is 2D list num outputs x num classes
      Y := To_Value_2D_List (Iris_Data.Label_Values);
      Assert (Integer (Y.Length) = Num_Samples, Routine_Name &
                " invalid Y vector");
      --  L1695
      Classification_Fit (theClassifier, X, Y, No_Weights);
--        Print_Tree ("The Tree", theClassifier);
      Put_Line ("----------------------------------------------");
      New_Line;

      Graphviz_Exporter.C_Init
        (Exporter, theClassifier.Attributes.Decision_Tree);
      Graphviz_Exporter.Export_Graphviz
        (Exporter, theClassifier.Attributes.Decision_Tree,
         Output_File_Name => To_Unbounded_String ("decision_path.dot"));

   end Test_Decision_Path;

   --  -------------------------------------------------------------------------

end Decision_Path_Tests;
