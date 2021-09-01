--  Based on scikit-learn/sklearn/utils/tests

with Ada.Text_IO; use Ada.Text_IO;

with Classifier_Types;
with Classifier_Utilities; use Classifier_Utilities;
with Encode_Utils;
with Weights;

package body Class_Weight_Tests is

   procedure Test_Compute_Class_Weight (Y : List_Of_Value_Data_Lists) is
      use Classifier_Types;
      use Float_Package;
      use Natural_Package;
      No_Weights    : constant Weight_List := Float_Package.Empty_Vector;
      Y_1           : Value_Data_List;
      Class_Weights : Weight_List;
      Classes       : Value_Data_List;
      Class_Counts  : Natural_List;
      Labels        : Natural_List := Natural_Package.Empty_Vector;
      Dot_Product   : Float;
      OK            : Boolean;
   begin
      New_Line;
      Put_Line ("Class Weight Tests");
      if Integer (Y.Length) = 0 then
         raise Class_Weight_Error with
         "Class_Weight_Tests.Test_Compute_Class_Weight called with empty data vector.";
      end if;

      Put_Line ("Y length: " & Integer'Image (Integer (Y.Length)));

      Print_Multi_Value_List ("Y", Y);
      Y_1 := Y.Element (1);
      Classes := Encode_Utils.Unique (Y_1, Labels);
      Print_Value_List ("Test_Compute_Class_Weight, Classes", Classes);

      Class_Weights := Weights.Compute_Class_Weights
        (Weights.Balanced_Weight, No_Weights, Classes, Y_1);
      Class_Counts := Classifier_Utilities.Bin_Count (Y_1);
      Delete_First (Class_Counts, 2);
      Dot_Product := Dot (Class_Weights, Class_Counts);
      OK := Integer (Dot_Product + 10.0 ** (-10)) = Integer (Y.Length);
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
         Print_Weights ("Weights", Class_Weights);
      end if;

   end Test_Compute_Class_Weight;

   --  ------------------------------------------------------------------------

   procedure Test_Compute_Sample_Weight (Weight_Kind : Weights.Weight_Type;
                                         Y           : List_Of_Value_Data_Lists) is
      use Classifier_Types;
      use Float_Package;
      use Weights;
      No_Weights     : constant Weight_List := Float_Package.Empty_Vector;
      Y_1            : constant Value_Data_List := Y.Element (1);
      Labels         : Natural_List;
      Classes        : constant Value_Data_List :=
                         Encode_Utils.Unique (Y_1, Labels);
      Class_Weights  : constant Weight_List := Compute_Class_Weights
        (Weight_Kind, No_Weights, Classes, Y_1);
      Sample_Weights : Weight_List;
      --          OK            : Boolean;
   begin
      Put_Line ("Class_Weight_Tests.Test_Compute_Sample_Weight");
      Sample_Weights := Compute_Sample_Weight (Weight_Kind, Y_1, Class_Weights);

      Print_Value_List ("    Y", Y_1);
      Print_Weights ("Weights", Sample_Weights);

   end Test_Compute_Sample_Weight;

end Class_Weight_Tests;
