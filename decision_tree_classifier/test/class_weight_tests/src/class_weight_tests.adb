
with Classifier_Utilities; use Classifier_Utilities;
with ML_Types; use ML_Types;
with Weights; use Weights;

package body Class_Weight_Tests is

    procedure Test_Compute_Class_Weight is
        Y             : ML_Types.Value_Data_List;
        Value         : Value_Record (Integer_Type);
        Class_Weights : Weight_List;
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

        Class_Weights := Unique_Values (Y);

    end Test_Compute_Class_Weight;

end Class_Weight_Tests;
