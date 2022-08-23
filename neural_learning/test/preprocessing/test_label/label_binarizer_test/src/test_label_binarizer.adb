--  Based on scikit-learn/sklearn/preprocessing/tests/test_label.py

with Ada.Assertions; use  Ada.Assertions;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;

with Label;
with NL_Types; use NL_Types;
with NL_Arrays_And_Matrices; use NL_Arrays_And_Matrices;
with Test_Support;

procedure Test_Label_Binarizer is
    use Unbounded_Package;
    Routine_Name  : constant String := "Test_Label_Binarizer ";
    Pos           : constant Unbounded_String := To_Unbounded_String ("pos");
    Inp           : constant Unbounded_String_Array (1 .. 4) :=
                      (Pos, Pos, Pos, Pos);
    Expected_Classes : Unbounded_List;
    --  one class case should default to negative label (0)
    Expected      : constant Binary_Matrix (1 .. 4, 1 .. 1) :=
    ((1 => 0), (1 => 0), (1 => 0), (1 => 0));
    Got           : Binary_Matrix (1 .. 4, 1 .. 1);
    LB            : Label.UB_Label_Binarizer;
begin
    Put_Line (Routine_Name);
    Expected_Classes.Append (Pos);
    Got := Label.Fit_Transform (LB, Inp);
    Test_Support.Print_Unbound_List ("Classes", LB.Classes);
    Assert (LB.Classes = Expected_Classes, "Unexpected classes");
--      Test_Support.Print_Binary_Matrix ("Got", Got);
    Assert (Got = Expected, "Got invalid data");
--      Test_Support.Print_Binary_Matrix ("Got", Label.Inverse_Transform (LB, Got));
    Put_Line (Routine_Name & "tests passed.");

end Test_Label_Binarizer;
