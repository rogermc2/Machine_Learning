
--  with Ada.Text_IO; use Ada.Text_IO;

--  with Classifier_Types;
--  with Classifier_Utilities; use Classifier_Utilities;
with Label;

package body Label_Tests is

   procedure Test_Label_Encoder is
      --        use Classifier_Types;
      --        use Float_Package;
      --        use Natural_Package;
   begin
      null;
   end Test_Label_Encoder;

   --  -------------------------------------------------------------------------

   procedure Test_Label_Encoder_Empty_Array
      (Values : ML_Types.Value_Data_List) is
      --        use Classifier_Types;
      --        use Float_Package;
      --        use Natural_Package;
      use Label;
      LE  : Label_Encoder;
   begin
      LE := Fit (Values);
   end Test_Label_Encoder_Empty_Array;

end Label_Tests;
