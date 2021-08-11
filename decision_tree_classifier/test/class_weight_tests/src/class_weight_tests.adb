
with Classifier_Types;
with Classifier_Utilities; use Classifier_Utilities;
with ML_Types; use ML_Types;
with Weights;

package body Class_Weight_Tests is

    procedure Test_Compute_Class_Weight is
        Y             : Value_Data_List;
        Value         : Value_Record (Integer_Type);
        Class_Weights : Weights.Weight_List;
        Classes       : Value_Data_List;
        Class_Counts  : Classifier_Types.Natural_List;
        No_Weights    : Weights.Weight_List;
    begin
        Value.Integer_Value := 2;
        Y.Append (Value);
        Y.Append (Value);
        Y.Append (Value);
        Value.Integer_Value := 3;
        Y.Append (Value);
        Y.Append (Value);
        Value.Integer_Value := 4;
        Y.Append (Value);

        Classes := Unique_Values (Y);
        Class_Weights := Weights.Compute_Class_Weights
          (Weights.Balanced_Weight, No_Weights, Classes, Y);
         Class_Counts := Classifier_Utilities.Bin_Count (Y);
    end Test_Compute_Class_Weight;

end Class_Weight_Tests;
