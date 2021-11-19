
with Ada.Assertions; use Ada.Assertions;
with Ada.Text_IO; use Ada.Text_IO;

with Base_Decision_Tree;
with Classifier_Types;
with Classifier_Utilities;
with Criterion;
with Decision_Tree_Classification;
with ML_Types;
with Node_Splitter;
--  with Printing;
with Tree;
with Weights;

package body Export_Tests is
   use Classifier_Types;
   use ML_Types;
   X_Array     : constant Multi_Value_Array (1 .. 6, 1 .. 2) :=
                   ((-2, -1), (-1, -1), (-1, -2), (1, 1), (1, 2), (2, 1));
   --  Y_Array 6 rows (samples) x 2 columns (features)
   Y_Array     : constant Integer_Array (1 .. 6) := (-1, -1, -1, 1, 1, 1);
   Y2_Array    : constant Multi_Value_Array (1 .. 6, 1 .. 2) :=
                    ((-1, 1), (-1, 1), (-1, 1), (1, 2), (1, 2), (1, 3));
   W_Array     : constant Float_Array (1 .. 6) :=
                    (1.0, 1.0, 1.0, 0.5, 0.5, 0.5);
   Y_Deg_Array : constant Integer_Array (1 .. 6) := (1, 1, 1, 1, 1, 1);

   --  -------------------------------------------------------------------------

   procedure Test_Graphviz_Toy  is
      use Classifier_Utilities;
      use Decision_Tree_Classification;
      use Float_Package;
      Routine_Name     : constant String :=
                           "Classification_Tests.Test_Classification_Toy";
      Criteria         : Criterion.Criterion_Class;
      Splitter         : Node_Splitter.Splitter_Class;
      theClassifier    : Base_Decision_Tree.Classifier
        (Tree.Integer_Type, Tree.Integer_Type, Tree.Integer_Type);
      X                : constant Value_Data_Lists_2D :=
                           To_Multi_Value_List (X_Array);
      Y                : Value_Data_Lists_2D :=
                             To_Integer_Value_List_2D (Y_Array);
      --  Y2 is 2D list num outputs x num classes
      Y2               : constant Value_Data_Lists_2D :=
                           To_Multi_Value_List (Y2_Array);
      W                : Weights.Weight_List := To_Float_List (W_Array);
      Y_Degraded       : Value_Data_Lists_2D :=
                              To_Integer_Value_List_2D  (Y_Deg_Array);
      Num_Samples      : constant Natural := Natural (X.Length);
      No_Weights       : Weights.Weight_List := Empty_Vector;
   begin
      C_Init (theClassifier, Criteria, Splitter, Max_Depth => 3,
              Min_Split_Samples => 2);
      Put_Line (Routine_Name);
      Assert (Num_Samples > 0,
              Routine_Name & " called with empty X vector.");
      Classification_Fit (theClassifier, X, Y, No_Weights);

   end Test_Graphviz_Toy;

   --  -------------------------------------------------------------------------

end Export_Tests;
