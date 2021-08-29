--  Based on scikit-learn/sklearn/utils/tests

with Classifier_Utilities; use Classifier_Utilities;
with Class_Weight_Tests;

procedure Test_Weight_Functions is
begin
   Class_Weight_Tests.Test_Compute_Class_Weight
      (To_Integer_Value_List ((2, 2, 2, 3, 3, 4)));
   Class_Weight_Tests.Test_Compute_Sample_Weight
      (To_Integer_Value_List ((1, 1, 1, 2, 2, 2)));
end Test_Weight_Functions;
