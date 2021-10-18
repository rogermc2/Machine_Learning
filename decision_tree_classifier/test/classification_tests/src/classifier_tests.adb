
with Ada.Assertions; use Ada.Assertions;
with Ada.Text_IO; use Ada.Text_IO;

with Base_Decision_Tree;
with Classifier_Types;
with Classifier_Utilities;
with Decision_Tree_Classifer;
with ML_Types;
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
      Max_Depth       : constant Positive := 5;
      X               : constant List_Of_Value_Data_Lists :=
                          To_Multi_Value_List (X_Array);
      Y               : List_Of_Value_Data_Lists;
      Y_Encoded       : Classifier_Types.List_Of_Natural_Lists;
      Num_Samples     : constant Natural := Natural (X.Length);
      Classes         : ML_Types.List_Of_Value_Data_Lists;
      Weight_Values   : Weights.Weight_List;
      Probabilities   : List_Of_Value_Data_Lists;
   begin
      Put_Line ("Classification_Tests.Test_Classification_Toy:");
      Assert (Num_Samples > 0,
              "Classification_Tests.Test_Classification_Toy called with empty X vector.");

      Y := To_Integer_Value_List (Y_Array);
      --  L229
      --        Expected := To_Integer_Value_List (True_Result);
      --  L230
      Classification_Fit (theTree, X, Y, Y_Encoded, Classes, Weight_Values,
                          Max_Depth);
      Put_Line ("Classification_Tests.Test_Classification_Toy Tree size: " &
                  Integer'Image (Integer
                  (theTree.Attributes.Decision_Tree.Nodes.Node_Count) - 1));
      Print_Tree ("The Tree", theTree);
      Print_Float_List ("Classification_Tests.Test_Classification_Toy weights",
                        Weight_Values);
      Probabilities := Predict_Probability (theTree, X);
      Classifier_Utilities.Print_List_Of_Value_Data_Lists
        ("Classification_Tests.Test_Classification_Toy Probabilities",
         Probabilities);
      --          Print_Value_List ("Classification_Tests.Test_Classification_Toy Classes",
      --                            Classes);

   end Test_Classification_Toy;

   --  -------------------------------------------------------------------------

end Classifier_Tests;
