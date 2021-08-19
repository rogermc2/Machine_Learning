
with ML_Types;

with Label_Tests;

procedure Test_Labels is
begin
   Label_Tests.Test_Label_Encoder_Empty_Array
      (ML_Types.Value_Data_Package.Empty_Vector);
end Test_Labels;
