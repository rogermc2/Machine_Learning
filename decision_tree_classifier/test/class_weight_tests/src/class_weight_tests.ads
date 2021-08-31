
with ML_Types; use ML_Types;
with Weights;

package Class_Weight_Tests is

   procedure Test_Compute_Class_Weight (Y : Value_Data_List);
   procedure Test_Compute_Sample_Weight (Weight_Kind    : Weights.Weight_Type;
                                         Y              : Value_Data_List);

end Class_Weight_Tests;
