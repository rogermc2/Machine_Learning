
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
      use ML_Types;
      use Value_Data_Package;
      use Natural_Package;
      use Label;
      Empty_List          : constant Natural_List :=
                                Natural_Package.Empty_Vector;
      LE                  : Label_Encoder;
      Transformed         : Natural_List;
      Inverse_Transformed : ML_Types.Value_Data_List;
   begin
      Fit (LE, Values);
      --  test empty transform
      Transformed := Transform (Values);
      Put ("Label_Tests.Test_Label_Encoder_Empty_Array: ");
      if Transformed = Natural_Package.Empty_Vector then
         Put_Line ("Empty transform test passed");
      else
         Put_Line ("Empty transform test failed");
      end if;

      --  test empty inverse transform
      Inverse_Transformed := Inverse_Transform (LE, Empty_List);
      Put ("Label_Tests.Test_Label_Encoder_Empty_Array: ");
      if Inverse_Transformed = Value_Data_Package.Empty_Vector then
         Put_Line ("Empty inverse transform test passed");
      else
         Put_Line ("Empty inverse transform test failed");
      end if;

   end Test_Label_Encoder_Empty_Array;

   --  -------------------------------------------------------------------------

   procedure Test_Label_Encoder_Negative_Integers is
      use ML_Types;
      use Label;
      use Classifier_Utilities;
      LE     : Label_Encoder;
      Values : Value_Data_List;
--        Val    : Value_Record (Integer_Type);
      Transformed  : Classifier_Types.Natural_List;
   begin
--        Val.Integer_Value := 1;
--        Values.Append (Val);
--        Values.Append (Val);
--        Val.Integer_Value := 4;
--        Values.Append (Val);
--        Val.Integer_Value := 5;
--        Values.Append (Val);
--        Val.Integer_Value := -1;
--        Values.Append (Val);
--        Val.Integer_Value := 0;
--        Values.Append (Val);
      Values := To_Integer_Value_List ((1, 4, 5, -1, 0));
      Fit (LE,Values);
      Print_Value_List
        ("Label_Tests Test_Label_Encoder_Negative_Integers Classes",
         LE.Classes);

      Transformed := Transform
          (To_Integer_Value_List ((0, 1, 4, 4, 5,  -1, -1)));
      Print_Natural_List
        ("Label_Tests Test_Label_Encoder_Negative_Integers transformed data",
         Transformed);
   end Test_Label_Encoder_Negative_Integers;

   --  -------------------------------------------------------------------------

end Label_Tests;
