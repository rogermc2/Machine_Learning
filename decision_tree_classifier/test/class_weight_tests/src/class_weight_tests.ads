
with ML_Types; use ML_Types;
with Weights;

package Class_Weight_Tests is

   Class_Weight_Error : Exception;

   procedure Test_Compute_Class_Weight (Y : Value_Data_Lists_2D);
   procedure Test_Compute_Sample_Weight
     (Weight_Kind : Weights.Weight_Type; Y : Value_Data_Lists_2D;
      Expected_Weights : Weights.Weight_List);

end Class_Weight_Tests;
