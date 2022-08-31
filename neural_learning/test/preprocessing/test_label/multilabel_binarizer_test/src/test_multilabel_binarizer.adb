--  Based on scikit-learn/sklearn/preprocessing/tests/test_label.py
--  test_multilabel_binarizer()

with Ada.Assertions; use  Ada.Assertions;
with Ada.Text_IO; use Ada.Text_IO;

with Label;
with NL_Types;
with NL_Arrays_And_Matrices; use NL_Arrays_And_Matrices;
--  with Test_Support;

procedure Test_Multilabel_Binarizer is
   use NL_Types.Integer_Package;
   use Integer_Array_Package;
   Routine_Name     : constant String := "Test_Multilabel_Binarizer ";
   Indicator_Mat    : constant Binary_Matrix (1 .. 3, 1 .. 3) :=
                        ((0, 1, 1), (1, 0, 0), (1, 1, 0));
   Indicator_Mat2    : constant Binary_Matrix (1 .. 3, 1 .. 4) :=
                        ((0, 0, 1, 1), (1, 1, 0, 0), (0, 1, 1, 0));
   lambda_Mat       : constant Integer_Matrix (1 .. 3, 1 .. 2) :=
                           ((2, 3), (0, 1), (1, 2));
   lambda1_1        : constant Integer_Array (1 .. 2) := (2, 3);
   lambda1_2        : constant Integer_Array (1 .. 1) := (1 => 1);
   lambda1_3        : constant Integer_Array (1 .. 2) := (1, 2);
   lambda1          : Integer_Array_List;
   Expected_Classes : NL_Types.Integer_List;
   Expected_Mat_Classes : NL_Types.Integer_List;
   MLB              : Label.Multi_Label_Binarizer;
begin
   Put_Line (Routine_Name);
   lambda1.Append (lambda1_1);
   lambda1.Append (lambda1_2);
   lambda1.Append (lambda1_3);
   Expected_Classes.Append (1);
   Expected_Classes.Append (2);
   Expected_Classes.Append (3);
   Expected_Mat_Classes := Expected_Classes;
   Expected_Mat_Classes.Prepend (0);

   --   fit case
   Label.Fit (MLB, lambda1);
   declare
      Got : constant Binary_Matrix := Label.Transform (lambda1);
   begin
      Assert (MLB.Classes = Expected_Classes, "fit case unexpected classes");
      Assert (Got = Indicator_Mat, "fit case Got invalid data");
      Assert (Label.Inverse_Transform (MLB, Got) = lambda1,
              "fit case invalid inverse Got");
   end;

   Label.Fit (MLB, lambda_Mat);
   declare
      Got : constant Binary_Matrix := Label.Transform (lambda1);
   begin
      Assert (MLB.Classes = Expected_Classes,
              "matrix fit case unexpected classes");
      Assert (Got = Indicator_Mat, "matrix fit case Got invalid data");
      Assert (Label.Inverse_Transform (MLB, Got) = lambda1,
              "matrix fit case invalid inverse Got");
   end;

   --  fit_transform case
   declare
      MLB2 : Label.Multi_Label_Binarizer;
      Got : constant Binary_Matrix := Label.Fit_Transform (MLB2, lambda1);
   begin
      Assert (MLB2.Classes = Expected_Classes,
              "fit_transform case unexpected classes");
      Assert (Got = Indicator_Mat, "fit_transform case Got invalid data");
      Assert (Label.Inverse_Transform (MLB2, Got) = lambda1,
              "fit_transform case invalid inverse Got");
   end;

   declare
      MLB2 : Label.Multi_Label_Binarizer;
      Got  : constant Binary_Matrix := Label.Fit_Transform (MLB2, lambda_Mat);
   begin
      Assert (MLB2.Classes = Expected_Mat_Classes,
              "matrix fit_transform case unexpected lambda_Mat classes");
      Assert (Got = Indicator_Mat2,
              "matrix fit_transform case Got invalid lambda_Mat data");
      Assert (Label.Inverse_Transform (MLB2, Got) = lambda_Mat,
              "matrix fit_transform case invalid inverse lambda_Mat Got");
   end;

   Put_Line (Routine_Name & "tests passed.");

end Test_Multilabel_Binarizer;
