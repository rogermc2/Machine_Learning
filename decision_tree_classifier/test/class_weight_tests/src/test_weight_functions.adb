--  Based on scikit-learn/sklearn/utils/tests

with Ada.Text_IO; use Ada.Text_IO;

with Classifier_Types;
with Classifier_Utilities; use Classifier_Utilities;
with Class_Weight_Tests;
with Weights;

procedure Test_Weight_Functions is
   BC3                     : constant Float := 7.0 / 9.0;
   BC1                     : constant Float := 7.0 / 3.0;
   BC3_Sq                  : constant Float := BC3 ** 2;
   BC1_Sq                  : constant Float := BC1 ** 2;
   Multi_Values            : Classifier_Types.Multi_Value_Array (1..6, 1..2);
   Multi_Values_UB         : Classifier_Types.Multi_Value_Array (1..7, 1..2);
   Expected_Sample_Weights : Classifier_Types.Weight_List;
begin
   Put_Line ("Class Weight Tests");
   Class_Weight_Tests.Test_Compute_Class_Weight
     (To_Integer_Value_List ((2, 2, 2, 3, 3, 4)));
   New_Line;

   Put_Line ("Sample Weight Tests");
   Put_Line ("Balanced sample weight tests");
   Expected_Sample_Weights := To_Float_List ((1.0, 1.0, 1.0, 1.0, 1.0, 1.0));
   Class_Weight_Tests.Test_Compute_Sample_Weight
     (Weights.Balanced_Weight, To_Integer_Value_List ((1, 1, 1, 2, 2, 2)),
      Expected_Sample_Weights);
   New_Line;

   --  Test with unbalanced classes
   Put_Line ("Unbalanced sample weight tests");
   --  Samples length 7
   --  Number of classes 3
   --  Bin counts 3, 3, 1
   --  Weight (index) = Samples Length / (Classes Length * Bin_Count (index))
   --  Bin count 3: 7 / (3 * 3) = 0.7777.....
   --  Bin count 1: 7 / (3 * 1) = 2.3333.....
   Expected_Sample_Weights := To_Float_List
     ((BC3, BC3, BC3, BC3, BC3, BC3, BC1));
   Class_Weight_Tests.Test_Compute_Sample_Weight
     (Weights.Balanced_Weight, To_Integer_Value_List ((1, 1, 1, 2, 2, 2, 3)),
      Expected_Sample_Weights);
   New_Line;

   --  Test with `None` weights
   Put_Line ("'None' sample weight tests");
   Expected_Sample_Weights := To_Float_List ((1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0));
   Class_Weight_Tests.Test_Compute_Sample_Weight
     (Weights.No_Weight, To_Integer_Value_List ((1, 1, 1, 2, 2, 2, 3)),
      Expected_Sample_Weights);
   New_Line;

   --  Test with multi-output of balanced classes
   Put_Line ("Multi-output sample weights balanced classes tests");
   Multi_Values := ((1, 0), (1, 0), (1, 0), (2, 1), (2, 1), (2, 1));
   Expected_Sample_Weights := To_Float_List ((1.0, 1.0, 1.0, 1.0, 1.0, 1.0));
   Class_Weight_Tests.Test_Compute_Sample_Weight
     (Weights.Balanced_Weight, To_Multi_Value_List (Multi_Values),
      Expected_Sample_Weights);
   New_Line;

   --  Test with multi-output of unbalanced classes
   Put_Line ("Multi-output sample weights unbalanced classes tests");
   Multi_Values_UB := ((1, 0), (1, 0), (1, 0), (2, 1), (2, 1), (2, 1), (3, -1));
   Expected_Sample_Weights := To_Float_List
     ((BC3_Sq, BC3_Sq, BC3_Sq, BC3_Sq, BC3_Sq, BC3_Sq, BC1_Sq));
   Class_Weight_Tests.Test_Compute_Sample_Weight
     (Weights.Balanced_Weight, To_Multi_Value_List (Multi_Values_UB),
      Expected_Sample_Weights);

end Test_Weight_Functions;
