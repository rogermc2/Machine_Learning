
with Ada.Text_IO; use Ada.Text_IO;

with Classifier_Types;
with Classifier_Utilities; use Classifier_Utilities;
with ML_Types; use ML_Types;
with Weights;

package body Class_Weight_Tests is

   procedure Test_Compute_Class_Weight is
      use Classifier_Types.Float_Package;
      Y             : Value_Data_List;
      Value         : Value_Record (Integer_Type);
      Class_Weights : Classifier_Types.Weight_List;
      Classes       : Value_Data_List;
      Class_Counts  : Classifier_Types.Natural_List;
      No_Weights    : Classifier_Types.Weight_List;
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

      Classes := Unique_Values (Y);
      Class_Weights := Weights.Compute_Class_Weights
        (Weights.Balanced_Weight, No_Weights, Classes, Y);
      Class_Counts := Classifier_Utilities.Bin_Count (Y);
      Dot_Product := Dot (Class_Weights, Class_Counts);
      Print_Natural_List ("Class_Weight_Test Class_Counts", Class_Counts);
      Print_Weights ("Class_Weight_Test Class_Weights", Class_Weights);
      Put_Line ("Class_Weight_Test Dot_Product: " &
                  Float'Image (Dot_Product) & ", Y length: " &
                  Integer'Image (Integer (Y.Length)));

   end Test_Compute_Class_Weight;

end Class_Weight_Tests;
