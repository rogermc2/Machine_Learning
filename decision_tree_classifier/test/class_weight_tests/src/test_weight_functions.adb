--  Based on scikit-learn/sklearn/utils/tests

with Ada.Text_IO; use Ada.Text_IO;

with Classifier_Types;
with Classifier_Utilities; use Classifier_Utilities;
with Class_Weight_Tests;
with Weights;

procedure Test_Weight_Functions is
    Multi_Values : Classifier_Types.Multi_Value_Array (1..6, 1..2);
begin
   Class_Weight_Tests.Test_Compute_Class_Weight
     (To_Integer_Value_List ((2, 2, 2, 3, 3, 4)));
   Class_Weight_Tests.Test_Compute_Sample_Weight
     (Weights.Balanced_Weight, To_Integer_Value_List ((1, 1, 1, 2, 2, 2)));
   New_Line;
   Put_Line ("Sample Weight Tests");
   Put_Line ("Balanced Sample Weight Tests");
   --  Test with unbalanced classes
   Class_Weight_Tests.Test_Compute_Sample_Weight
     (Weights.Balanced_Weight, To_Integer_Value_List ((1, 1, 1, 2, 2, 2, 3)));
   New_Line;
   Put_Line ("Sample Weight, 'None' weights Tests");
   --  Test with `None` weights
   Class_Weight_Tests.Test_Compute_Sample_Weight
     (Weights.No_Weight, To_Integer_Value_List ((1, 1, 1, 2, 2, 2, 3)));
   --  Test with multi-output of balanced classes
   Multi_Values := ((1, 0), (1, 0), (1, 0), (2, 1), (2, 1), (2, 1));
   Class_Weight_Tests.Test_Compute_Sample_Weight
     (Weights.Weights_List, To_Multi_Value_List (Multi_Values));

end Test_Weight_Functions;
