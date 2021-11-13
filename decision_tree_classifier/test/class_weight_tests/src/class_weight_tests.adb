--  Based on scikit-learn/sklearn/utils/tests test_class_weight.py

with Ada.Assertions; use Ada.Assertions;
with Ada.Text_IO; use Ada.Text_IO;

with Classifier_Types;
with Classifier_Utilities; use Classifier_Utilities;
with Encode_Utils;
with Printing;
with Weights;

package body Class_Weight_Tests is

   procedure Test_Compute_Class_Weight (Y : Value_Data_Lists_2D) is
      use Classifier_Types;
      use Float_Package;
      use Natural_Package;
      No_Weights    : constant Weights.Weight_List := Float_Package.Empty_Vector;
      Values        : Value_Data_List;
      Class_Weights : Weights.Weight_List;
      Classes       : Value_Data_List;
      Class_Counts  : Natural_List;
      Labels        : Natural_List := Natural_Package.Empty_Vector;
      Dot_Product   : Float;
      OK            : Boolean;
   begin
      New_Line;
      Assert (not Y.Is_Empty,
           "Class_Weight_Tests.Test_Compute_Class_Weight called with " &
           "empty data vector.");

      for index in Y.First_Index .. Y.Last_Index loop
            Values.Append (Y.Element (index));
      end loop;

      Classes := Encode_Utils.Unique (Values, Labels);

      Class_Weights := Weights.Compute_Class_Weights
        (Weights.Balanced_Weight, No_Weights, Classes, Values);
      Class_Counts := Classifier_Utilities.Bin_Count (Values);
      Delete_First (Class_Counts, 2);
      Dot_Product := Dot (Class_Weights, Class_Counts);
      OK := Integer (Dot_Product + 10.0 ** (-10)) = Integer (Values.Length);
      OK := OK and (Class_Weights.Element (1) < Class_Weights.Element (2)) and
        (Class_Weights.Element (2) < Class_Weights.Element (3));

      Put ("Test_Compute_Class_Weight, Weights test ");
      if OK then
         Put_Line ("passed.");
      else
         Put_Line ("failed.");
         Put_Line ("Test_Compute_Class_Weight, Dot_Product: " &
                     Float'Image (Dot_Product)
                   & ", Y length: " & Integer'Image (Integer (Y.Length)));
         Printing.Print_Weights ("Weights", Class_Weights);
      end if;

   end Test_Compute_Class_Weight;

   --  ------------------------------------------------------------------------

   procedure Test_Compute_Sample_Weight
     (Weight_Kind      : Weights.Weight_Type; Y : Value_Data_Lists_2D;
      Expected_Weights : Weights.Weight_List) is
      use Classifier_Types.Float_Package;
      use Weights;
      Sample_Weights   : Weight_List;
   begin
      Sample_Weights := Compute_Sample_Weight (Weight_Kind, Y);
      New_Line;
      Put ("Test_Compute_Class_Weight, Weights test ");
      if Classifier_Utilities.Compare_Float_Lists
        (Sample_Weights, Expected_Weights) then
         Put_Line ("passed.");
      else
         Put_Line ("failed.");
         Printing.Print_Weights ("Expected", Expected_Weights);
         Printing.Print_Weights ("Weights", Sample_Weights);
      end if;

   end Test_Compute_Sample_Weight;

end Class_Weight_Tests;
