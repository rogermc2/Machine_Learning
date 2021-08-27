--  Based on scikit-learn/sklearn/utils/tests

with Ada.Text_IO; use Ada.Text_IO;

with Classifier_Types;
with Classifier_Utilities; use Classifier_Utilities;
with Encode_Utils;
with ML_Types; use ML_Types;
with Weights;

package body Class_Weight_Tests is

   procedure Test_Compute_Class_Weight is
      use Classifier_Types;
      use Float_Package;
      use Natural_Package;
      Y             : Value_Data_List;
      Value         : Value_Record (Integer_Type);
      Class_Weights : Weight_List;
      Classes       : Value_Data_List;
      Class_Counts  : Natural_List;
      No_Weights    : constant Weight_List := Float_Package.Empty_Vector;
      Inverse       : Natural_List := Natural_Package.Empty_Vector;
      Dot_Product   : Float;
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

      New_Line;
      Put_Line ("Class_Weight_Tests");
      Classes := Encode_Utils.Unique (Y, Inverse);
      Print_Value_List ("Test_Compute_Class_Weight, Classes", Classes);
      New_Line;
      Class_Weights := Weights.Compute_Class_Weights
        (Weights.Balanced_Weight, No_Weights, Classes, Y);
      Print_Weights ("Test_Compute_Class_Weight, Class_Weights", Class_Weights);
      New_Line;
      Class_Counts := Classifier_Utilities.Bin_Count (Y);
      Print_Natural_List ("Test_Compute_Class_Weight, Class_Counts", Class_Counts);
      Delete_First (Class_Counts, 2);
      Dot_Product := Dot (Class_Weights, Class_Counts);
      Put_Line ("Test_Compute_Class_Weight, Dot_Product: " & Float'Image (Dot_Product)
                 & ", Y length: " & Integer'Image (Integer (Y.Length)));

   end Test_Compute_Class_Weight;

end Class_Weight_Tests;
