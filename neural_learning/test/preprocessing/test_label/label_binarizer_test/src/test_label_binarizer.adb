--  Based on scikit-learn/sklearn/preprocessing/tests/test_label.py

with Ada.Assertions; use  Ada.Assertions;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;

with Label;
with NL_Types; use NL_Types;
with NL_Arrays_And_Matrices; use NL_Arrays_And_Matrices;
--  with Test_Support;

procedure Test_Label_Binarizer is
   use Unbounded_Package;
   Routine_Name     : constant String := "Test_Label_Binarizer ";
   Neg              : constant Unbounded_String := To_Unbounded_String ("neg");
   Pos              : constant Unbounded_String := To_Unbounded_String ("pos");
   Inp1             : constant Unbounded_String_Array (1 .. 4) :=
                        (Pos, Pos, Pos, Pos);
   Inp2             : constant Unbounded_String_Array (1 .. 4) :=
                        (Neg, Pos, Pos, Neg);
   Expected_Classes : Unbounded_List;
   --  one class case should default to negative label (0)
   Expected1        : constant Binary_Matrix (1 .. 4, 1 .. 1) :=
                        ((1 => 0), (1 => 0), (1 => 0), (1 => 0));
   Expected2        : constant Binary_Matrix (1 .. 4, 1 .. 1) :=
                        ((1 => 0), (1 => 1), (1 => 1), (1 => 0));
   Got             : Binary_Matrix (1 .. 4, 1 .. 1);
--     Got2             : Binary_Matrix (1 .. 4, 1 .. 2);
   LB               : Label.UB_Label_Binarizer;
begin
   Put_Line (Routine_Name);
   --  One class case should default to negative label
   Expected_Classes.Append (Pos);
   Got := Label.Fit_Transform (LB, Inp1);
   Assert (LB.Classes = Expected_Classes, "Unexpected classes");
   Assert (Got = Expected1, "Got1 invalid data");
   Assert (Label.Inverse_Transform (LB, Got) = Inp1, "invalid inverse Got");

   --  Two classes case
   Expected_Classes.Prepend (Neg);
--     Test_Support.Print_Binary_Matrix ("Fit_Transform",
--                                       Label.Fit_Transform (LB, Inp2));
   Got := Label.Fit_Transform (LB, Inp2);
   Assert (LB.Classes = Expected_Classes, "Unexpected classes");
--     Test_Support.Print_Binary_Matrix ("Got2", Got);
   Assert (Got = Expected2, "Got2 invalid data");

   Put_Line (Routine_Name & "tests passed.");

end Test_Label_Binarizer;
