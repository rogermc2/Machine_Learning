
--  with Ada.Text_IO; use Ada.Text_IO;
with ML_Types;
with Classifier_Utilities;

with Label_Tests; use Label_Tests;

procedure Test_Labels is
begin
   Test_Label_Encoder_Empty_Array
     (ML_Types.Value_Data_Package.Empty_Vector);
   Test_Label_Encoder_Negative_Integers;
   Test_Label_Encoder
      (Classifier_Utilities.To_Integer_Value_List ((1, 4, 5, -1, 0)),
       Classifier_Utilities.To_Integer_Value_List ((1, 4, 5, -1, 0)),
       Classifier_Utilities.To_Natural_List ((1, 2, 3, 3, 4, 0, 0)));

end Test_Labels;
