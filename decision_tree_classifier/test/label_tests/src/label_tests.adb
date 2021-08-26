
with Ada.Text_IO; use Ada.Text_IO;

with Classifier_Utilities;
with Encode_Utils;
with Label;

package body Label_Tests is

   --  -------------------------------------------------------------------------

   procedure Test_Label_Encoder (Values  : ML_Types.Value_Data_List;
                                 Classes : Classifier_Types.Natural_List) is
      use ML_Types.Value_Data_Package;
      use Classifier_Utilities;
      --          use Classifier_Types.Natural_Package;
      use Label;
      Uniques : constant ML_Types.Value_Data_List := Encode_Utils.Unique (Values);
      LE_U    : Label_Encoder (Class_Unique);
      OK      : Boolean := True;
   begin
      Print_Value_List ("Values", Values);
      Print_Value_List ("Uniques", Uniques);
      Fit (LE_U, Values);
      for index in Uniques.First_Index .. Uniques.Last_Index loop
         OK := OK and LE_U.Uniques.Contains (Uniques.Element (index));
      end loop;
      Put ("Label_Tests.Test_Label_Encoder: ");
      if OK then
         Put_Line ("Class match test passed");
      else
         Put_Line ("Class match test failed");
         Print_Value_List ("Fit Uniques", LE_U.Uniques);
      end if;
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
      LE                  : Label_Encoder (Class_Unique);
      Transformed         : Natural_List;
      Inverse_Transformed : ML_Types.Value_Data_List;
   begin
      Fit (LE, Values);
      --  test empty transform
      Transformed := Transform (LE, Values);
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
      use Value_Data_Package;
      use Classifier_Types.Natural_Package;
      Expected_Uniques   : Value_Data_List;
      Expected_Labels    : constant Classifier_Types.Natural_List :=
                             To_Natural_List ((1, 2, 3, 3, 4, 0, 0));
      Values             : constant Value_Data_List :=
                             To_Integer_Value_List ((1, 4, 5, -1, 0));
      LE_U               : Label_Encoder (Class_Unique);
      Transformed        : Classifier_Types.Natural_List;
   begin
      Expected_Uniques := To_Integer_Value_List ((-1, 0, 1, 4, 5 ));
      New_Line;
      Fit (LE_U, Values);
      Put
        ("Label_Tests Test_Label_Encoder_Negative_Integers, Uniques test ");
      if LE_U.Uniques = Expected_Uniques then
         Put_Line ("passed");
      else
         Put_Line ("failed");
         Print_Value_List ("Transformed data", LE_U.Uniques);
         Print_Value_List ("Expected", Expected_Uniques);
      end if;

      Transformed := Transform
        (LE_U, To_Integer_Value_List ((0, 1, 4, 4, 5, -1, -1)));
      Put
        ("Label_Tests Test_Label_Encoder_Negative_Integers, Labels test ");
      if Transformed = Expected_Labels then
         Put_Line ("passed");
      else
         Put_Line ("failed");
         Print_Natural_List ("Transformed data", Transformed);
         Print_Natural_List ("Expected", Expected_Labels);
      end if;

   end Test_Label_Encoder_Negative_Integers;

   --  -------------------------------------------------------------------------

end Label_Tests;
