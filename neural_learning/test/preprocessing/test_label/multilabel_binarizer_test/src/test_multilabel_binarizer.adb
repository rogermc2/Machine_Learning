--  Based on scikit-learn/sklearn/preprocessing/tests/test_label.py
--  test_multilabel_binarizer()

with Ada.Assertions; use  Ada.Assertions;
with Ada.Text_IO; use Ada.Text_IO;

with Label;
with NL_Types;
with NL_Arrays_And_Matrices; use NL_Arrays_And_Matrices;
with Test_Support;

procedure Test_Multilabel_Binarizer is
    use NL_Types.Integer_Package;
    use Integer_Array_Package;
    Routine_Name     : constant String := "Test_Multilabel_Binarizer ";
    Indicator_Mat    : constant Binary_Matrix (1 .. 3, 1 .. 3) :=
                         ((0, 1, 1), (1, 0, 0), (1, 1, 0));
    --     lambda_Mat          : constant Integer_Matrix (1 .. 3, 1 .. 2) :=
    --                          ((2, 3), (1, 0), (1, 2));
    lambda1_1        : constant Integer_Array (1 .. 2) := (2, 3);
    lambda1_2        : constant Integer_Array (1 .. 1) := (1 => 1);
    lambda1_3        : constant Integer_Array (1 .. 2) := (1, 2);
    lambda1          : Integer_Array_List;
    Expected_Classes : NL_Types.Integer_List;
    --     Expected1        : constant Binary_Matrix (1 .. 4, 1 .. 1) :=
    --                          ((1 => 0), (1 => 0), (1 => 0), (1 => 0));
    --     Expected2        : constant Binary_Matrix (1 .. 4, 1 .. 1) :=
    --                          ((1 => 0), (1 => 1), (1 => 1), (1 => 0));
    --     Expected3        : constant Binary_Matrix (1 .. 5, 1 .. 4) :=
    --                          ((0, 0, 0, 1), (0, 0, 1, 0), (0, 1, 0, 0),
    --                           (0, 0, 1, 0), (1, 0, 0, 0));
    --     To_Invert        : constant Binary_Matrix (1 .. 4, 1 .. 2) :=
    --                          ((1, 0), (0, 1), (0, 1), (1, 0));
    --     Got2             : Binary_Matrix (1 .. 5, 1 .. 4);
begin
    Put_Line (Routine_Name);
    lambda1.Append (lambda1_1);
    lambda1.Append (lambda1_2);
    lambda1.Append (lambda1_3);
    Expected_Classes.Append (1);
    Expected_Classes.Append (2);
    Expected_Classes.Append (3);

    --  fit_transform case
    declare
        MLB : Label.Multi_Label_Binarizer;
        Got : constant Binary_Matrix := Label.Fit_Transform (MLB, lambda1);
    begin
        Test_Support.Print_Binary_Matrix ("Got", Got);
        Assert (MLB.Classes = Expected_Classes, "Unexpected classes");
        Assert (Got = Indicator_Mat, "Got invalid data");
        Assert (Label.Inverse_Transform (MLB, Got) = lambda1,
                "invalid inverse Got");
    end;

    --    fit_transform case
    declare
        MLB : Label.Multi_Label_Binarizer;
    begin
        Label.Fit (MLB, lambda1);
        declare
            Got : Binary_Matrix := Label.Transform (MLB, lambda1);
        begin
            Got := Label.Transform (MLB, Got);
            Test_Support.Print_Binary_Matrix ("Got", Got);
            Assert (MLB.Classes = Expected_Classes, "Unexpected classes");
--              Assert (Got1 = Indicator_Mat, "Got invalid data");
--              Assert (Label.Inverse_Transform (MLB, Got1) = lambda1,
--                      "invalid inverse Got");
        end;
    end;

    Put_Line (Routine_Name & "tests passed.");

    end Test_Multilabel_Binarizer;
