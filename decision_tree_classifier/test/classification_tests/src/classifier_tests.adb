
with Ada.Assertions; use Ada.Assertions;
with Ada.Text_IO; use Ada.Text_IO;

with Base_Decision_Tree;
with Classifier_Types;
with Classifier_Utilities;
with Decision_Tree_Classifer;
with ML_Types;
with Tree;

package body Classifier_Tests is
   use Classifier_Types;
   use ML_Types;

   --     type Clf_Criterions is (Gini, Entropy);
   --     type Reg_Criterions is (Squared_Error, Absolute_Error,
   --                             Friedman_Mse, Poisson);
   --     type Clf_Trees is (Decision_Tree_Classifier, Extra_Tree_Classifier);
   --     type Reg_Trees is (Decision_Tree_Regressor, Extra_Tree_Regressor);
   --  X_Array 6 rows (samples) x 2 columns (features)
   X_Array     : constant Multi_Value_Array (1 .. 6, 1 .. 2) :=
                   ((-2, -1), (-1, -1), (-1, -2), (1, 1), (1, 2), (2, 1));
   Y_Array     : constant Integer_Array (1 .. 6) := (-1, -1, -1, 1, 1, 1);
   --     T_Array     : constant Multi_Value_Array (1 .. 3, 1 .. 2) :=
   --                     ((-1, -1), (2, 2), (3, 2));
   --     True_Result : constant Integer_Array (1 .. 3) := (-1, 1, 1);

   --  -------------------------------------------------------------------------

   procedure Test_Classification_Toy  is
      use Classifier_Utilities;
      use Decision_Tree_Classifer;

      --        Expected        : List_Of_Value_Data_Lists;
      theTree         : Base_Decision_Tree.Classifier
        (Tree.Integer_Type, Tree.Integer_Type, Tree.Integer_Type);
--        Max_Depth       : constant Positive := 5;
      X               : constant Value_Data_Lists_2D :=
                          To_Multi_Value_List (X_Array);
      Y               : Value_Data_Lists_2D;
      Num_Samples     : constant Natural := Natural (X.Length);
      Probabilities   : ML_Types.Value_Data_Lists_3D;
   begin
      Put_Line ("Classification_Tests.Test_Classification_Toy:");
      Assert (Num_Samples > 0,
              "Classification_Tests.Test_Classification_Toy called with empty X vector.");

      Y := To_Integer_Value_List (Y_Array);
      --  L229
      --        Expected := To_Integer_Value_List (True_Result);
      --  L230
      Classification_Fit (theTree, X, Y);
      Put_Line ("Classification_Tests.Test_Classification_Toy Tree size: " &
                  Integer'Image (Integer
                  (theTree.Attributes.Decision_Tree.Nodes.Node_Count) - 1));
      Print_Tree ("The Tree", theTree);
      Probabilities := Predict_Probability (theTree, X);
      Classifier_Utilities.Print_Value_Data_List_3D
        ("Classification_Tests.Test_Classification_Toy Probabilities",
         Probabilities);
      --          Print_Value_List ("Classification_Tests.Test_Classification_Toy Classes",
      --                            Classes);

   end Test_Classification_Toy;

   --  -------------------------------------------------------------------------

end Classifier_Tests;
