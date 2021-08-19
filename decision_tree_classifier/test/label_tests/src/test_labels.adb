
with ML_Types;

with Label_Tests; use Label_Tests;

procedure Test_Labels is
begin
   Test_Label_Encoder_Empty_Array
     (ML_Types.Value_Data_Package.Empty_Vector);
   Test_Label_Encoder_Negative_Integers;
end Test_Labels;
