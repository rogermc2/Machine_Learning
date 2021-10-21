
with Classifier_Types;
with ML_Types; use ML_Types;
with Weights;

package Class_Weight_Tests is

   Class_Weight_Error : Exception;

   procedure Test_Compute_Class_Weight (Y : List_Of_Value_Data_Lists);
   procedure Test_Compute_Sample_Weight
     (Weight_Kind : Weights.Weight_Type; Y  : List_Of_Value_Data_Lists;
      Expected_Weights : Classifier_Types.Weight_List);

end Class_Weight_Tests;
