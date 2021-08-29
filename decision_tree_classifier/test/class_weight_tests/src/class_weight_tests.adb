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
        Y             : constant Value_Data_List :=
                          To_Integer_Value_List ((2, 2, 2, 3, 3, 4));
        Class_Weights : Weight_List;
        Classes       : Value_Data_List;
        Class_Counts  : Natural_List;
        No_Weights    : constant Weight_List := Float_Package.Empty_Vector;
        Labels        : Natural_List := Natural_Package.Empty_Vector;
        Dot_Product   : Float;
        OK            : Boolean;
    begin
        New_Line;
        Put_Line ("Class_Weight_Tests");
        Classes := Encode_Utils.Unique (Y, Labels);
        Print_Value_List ("Test_Compute_Class_Weight, Classes", Classes);

        Class_Weights := Weights.Compute_Class_Weights
          (Weights.Balanced_Weight, No_Weights, Classes, Y);
        Class_Counts := Classifier_Utilities.Bin_Count (Y);
        Delete_First (Class_Counts, 2);
        Dot_Product := Dot (Class_Weights, Class_Counts);
        OK := Integer (Dot_Product + 10.0 ** (-10)) = Integer (Y.Length);
        Put ("Test_Compute_Class_Weight, Weights test ");
        if OK then
            Put_Line ("passed.");
        else
            Put_Line ("failes.");
            Put_Line ("Test_Compute_Class_Weight, Dot_Product: " &
                        Float'Image (Dot_Product)
                      & ", Y length: " & Integer'Image (Integer (Y.Length)));
        end if;

    end Test_Compute_Class_Weight;

end Class_Weight_Tests;
