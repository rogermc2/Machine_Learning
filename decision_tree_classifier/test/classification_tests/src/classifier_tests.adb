
with Ada.Text_IO; use Ada.Text_IO;

with Classifier_Types;
with Classifier_Utilities;
with Decision_Tree_Classifer;
with Estimator;
with Tree;
with ML_Types;

package body Classifier_Tests is
   use Classifier_Types;
   use ML_Types;

--     type Clf_Criterions is (Gini, Entropy);
--     type Reg_Criterions is (Squared_Error, Absolute_Error,
--                             Friedman_Mse, Poisson);
--     type Clf_Trees is (Decision_Tree_Classifier, Extra_Tree_Classifier);
--     type Reg_Trees is (Decision_Tree_Regressor, Extra_Tree_Regressor);

   X_Array     : constant Multi_Value_Array (1 .. 6, 1 .. 2) :=
                   ((-2, -1), (-1, -1), (-1, -2), (1, 1), (1, 2), (2, 1));
   Y_Array     : constant Integer_Array (1 .. 6) := (-1, -1, -1, 1, 1, 1);
   T_Array     : constant Multi_Value_Array (1 .. 3, 1 .. 2) :=
                   ((-1, -1), (2, 2), (3, 2));
   True_Result : constant Integer_Array (1 .. 3) := (-1, 1, 1);

   --  -------------------------------------------------------------------------

   procedure Test_Classification_Toy  is
      use Classifier_Utilities;
      use Decision_Tree_Classifer;
      aTree    : Classifier (Tree.Integer_Type, Tree.Integer_Type, Tree.Integer_Type);
      Result   : Estimator.Estimator_Data (1, 2);
      X        : constant List_Of_Value_Data_Lists := To_Multi_Value_List (X_Array);
      Y        : List_Of_Value_Data_Lists;
      Expected : List_Of_Value_Data_Lists;
      Weights  : Weight_List;
   begin
      Put_Line ("Classification_Tests.Test_Classification_Toy:");
      Y := To_Integer_Value_List (Y_Array);
      Expected := To_Integer_Value_List (True_Result);
      Result := Fit (aTree, X, Y, Weights);
      Print_Float_List ("Classification_Tests.Test_Classification_Toy weights", Weights);

   end Test_Classification_Toy;

   --  -------------------------------------------------------------------------

   --  -------------------------------------------------------------------------

end Classifier_Tests;
