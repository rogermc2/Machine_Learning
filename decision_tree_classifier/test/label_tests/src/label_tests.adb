
with Ada.Text_IO; use Ada.Text_IO;

with Classifier_Types;
with Classifier_Utilities;
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
      use Classifier_Types;
      use Natural_Package;
      use Label;
      LE  : Label_Encoder;
      T   : Natural_List;
   begin
      Fit (LE, Values);
      --  test empty transform
      T := Transform (LE, Values);
      Put ("Label_Tests.Test_Label_Encoder_Empty_Array: ");
      if T = Empty_Vector then
         Put_Line ("Empty transform test passed");
      else
         Put_Line ("Empty transform test failed");
      end if;
      --  test empty inverse transform
      --        T := Inverse_Transform (LE, Values);
      --        Put ("Label_Tests.Test_Label_Encoder_Empty_Array: ");
      --        if T = Empty_Vector then
      --           Put_Line ("Empty inverse transform test passed");
      --        else
      --           Put_Line ("Empty inverse transform test failed");
      --        end if;

   end Test_Label_Encoder_Empty_Array;

   --  -------------------------------------------------------------------------

   procedure Test_Label_Encoder_Negative_Integers is
      use ML_Types;
      use Label;
      use Classifier_Utilities;
      LE     : Label_Encoder;
      Values : Value_Data_List;
      Val    : Value_Record (Integer_Type);
      --        C_Fit  : Natural_List;
   begin
      Val.Integer_Value := 1;
      Values.Append (Val);
      Values.Append (Val);
      Val.Integer_Value := 4;
      Values.Append (Val);
      Val.Integer_Value := 5;
      Values.Append (Val);
      Val.Integer_Value := -1;
      Values.Append (Val);
      Val.Integer_Value := 0;
      Values.Append (Val);
      Fit (LE,Values);
      --        C_Fit := LE.Classes;
      Print_Value_List
        ("Label_Tests Test_Label_Encoder_Negative_Integers Classes",
         LE.Classes);
   end Test_Label_Encoder_Negative_Integers;

   --  -------------------------------------------------------------------------

end Label_Tests;
