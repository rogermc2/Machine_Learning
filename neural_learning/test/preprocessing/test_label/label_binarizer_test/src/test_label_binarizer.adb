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
   Spam             : constant Unbounded_String := To_Unbounded_String ("spam");
   Ham              : constant Unbounded_String := To_Unbounded_String ("ham");
   Eggs             : constant Unbounded_String := To_Unbounded_String ("eggs");
   Zero             : constant Unbounded_String := To_Unbounded_String ("0");
   Inp1             : constant Unbounded_String_Array (1 .. 4) :=
                        (Pos, Pos, Pos, Pos);
   Inp2             : constant Unbounded_String_Array (1 .. 4) :=
                        (Neg, Pos, Pos, Neg);
   Inp3             : constant Unbounded_String_Array (1 .. 5) :=
                        (Spam, Ham, Eggs, Ham, Zero);
   Expected_Classes : Unbounded_List;
   --  one class case should default to negative label (0)
   Expected1        : constant Binary_Matrix (1 .. 4, 1 .. 1) :=
                        ((1 => 0), (1 => 0), (1 => 0), (1 => 0));
   Expected2        : constant Binary_Matrix (1 .. 4, 1 .. 1) :=
                        ((1 => 0), (1 => 1), (1 => 1), (1 => 0));
   Expected3        : constant Binary_Matrix (1 .. 5, 1 .. 4) :=
                        ((0, 0, 0, 1), (0, 0, 1, 0), (0, 1, 0, 0),
                         (0, 0, 1, 0), (1, 0, 0, 0));
   To_Invert        : constant Binary_Matrix (1 .. 4, 1 .. 2) :=
                        ((1, 0), (0, 1), (0, 1), (1, 0));
   Got              : Binary_Matrix (1 .. 4, 1 .. 1);
   Got2             : Binary_Matrix (1 .. 5, 1 .. 4);
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
   Got := Label.Fit_Transform (LB, Inp2);
   Assert (LB.Classes = Expected_Classes, "Unexpected classes");
   Assert (Got = Expected2, "Two classes case Got invalid data");

   Assert (Label.Inverse_Transform (LB, To_Invert) = Inp2,
           "invalid inverse To_Invert");

   --   multiclass case
   Expected_Classes.Clear;
   Expected_Classes.Append (Zero);
   Expected_Classes.Append (Eggs);
   Expected_Classes.Append (Ham);
   Expected_Classes.Append (Spam);
   Put_Line ("multiclass case");
   Got2 := Label.Fit_Transform (LB, Inp3);

   Assert (LB.Classes = Expected_Classes, "Unexpected classes");
   Assert (Got2 = Expected3, "multiclasss test Got2 invalid data");
   Assert (Label.Inverse_Transform (LB, Got2) = Inp3, "multiclassinvalid inverse Got2");

   Put_Line (Routine_Name & "tests passed.");

end Test_Label_Binarizer;
