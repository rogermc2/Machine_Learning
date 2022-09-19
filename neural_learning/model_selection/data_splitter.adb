
--  Based on scikit-learn/sklearn/model_selection/_split.py

with Ada.Assertions; use Ada.Assertions;
with Ada.Text_IO; use Ada.Text_IO;

with Utilities;
--  with Printing;

package body Data_Splitter is

   procedure Iterate_Indices
     (Self                        : Base_Shuffle_Data;
      Train_Indices, Test_Indices : out Integer_Array);

   --  -------------------------------------------------------------------------
   --  L1569 Shuffle_Split generates indices to split data rows into training
   --  and test sets
   procedure Base_Shuffle_Split
     (Self                        : Base_Shuffle_Data;
      Train_Indices, Test_Indices : out Integer_Array) is
   begin
      Iterate_Indices (Self, Train_Indices, Test_Indices);

   end Base_Shuffle_Split;

   --  -------------------------------------------------------------------------

   procedure Init_Base_Shuffle_Split
     (Self                   : in out Base_Shuffle_Data; Num_Splits : Natural;
      Train_Size, Test_Size  : Natural) is
   begin
      Self.Num_Splits := Num_Splits;
      Self.Train_Size := Train_Size;
      Self.Test_Size := Test_Size;

   end Init_Base_Shuffle_Split;

   --  -------------------------------------------------------------------------
   --  L1706
   procedure Iterate_Indices
     (Self                        : Base_Shuffle_Data;
      Train_Indices, Test_Indices : out Integer_Array) is
--        Routine_Name  : constant String := "Data_Splitter.Iterate_Indices ";
      Num_Train     : constant Natural := Self.Train_Size;
      Num_Test      : constant Natural := Self.Test_Size;
      Num_Samples   : constant Natural := Num_Train + Num_Test;
      Perms         : Integer_Array (1 .. Num_Samples);
   begin
      for index in 1 .. Num_Samples loop
         Perms (index) := index;
      end loop;

      --  L1716
      for index in 1 .. Self.Num_Splits loop
         Utilities.Permute (Perms);
         for test_index in 1 .. Num_Test loop
            Test_Indices (test_index) := Perms (test_index);
         end loop;

         for train_index in 1 .. Num_Train loop
            Train_Indices (train_index) := Perms (Num_Test + train_index);
         end loop;
      end loop;

   end Iterate_Indices;

   --  -------------------------------------------------------------------------
   --  L2325
   procedure Train_Test_Split
     (X                     : Real_Float_Matrix; Y : Integer_Array;
      Train_Size, Test_Size : Natural;
      Train_X               : out Real_Float_Matrix; Train_Y : out Integer_Array;
      Test_X                : out Real_Float_Matrix; Test_Y : out Integer_Array) is
      Routine_Name : constant String := "Data_Splitter.Train_Test_Split ";
      Num_Samples  : constant Positive := X'Length;
   begin
      Assert (Natural (Y'Length) = Num_Samples, Routine_Name &
                "Y length" & Integer'Image (Integer (Y'Length)) &
                " is different to X length" & Natural'Image (Num_Samples));

      for row in 1 .. Train_Size loop
         for col in Train_X'Range (2) loop
            Train_X (row, col) := X (row, col);
         end loop;
         Train_Y (row) := Y (row);
      end loop;

      for row in 1 .. Test_Size loop
         for col in Test_X'Range (2) loop
            Test_X (row, col) := X (row + Train_Size, col);
         end loop;
         Test_Y (row) := Y (row + Train_Size);
      end loop;

   end Train_Test_Split;

   --  -------------------------------------------------------------------------
   --  L2325 Integer Y
   procedure Train_Test_Split
     (X                     : Real_Float_Matrix; Y : Integer_Matrix;
      Train_Size, Test_Size : Natural;
      Train_X               : out Real_Float_Matrix; Train_Y : out Integer_Matrix;
      Test_X                : out Real_Float_Matrix; Test_Y : out Integer_Matrix) is
      Routine_Name      : constant String :=
                            "Data_Splitter.Train_Test_Split Integer ";
      Num_Samples       : constant Positive := Positive (X'Length);
      Train_Indices     : Integer_Array (1 .. Train_Size);
      Test_Indices      : Integer_Array (1 .. Test_Size);
      Shuffle_Data      : Base_Shuffle_Data
        (Train_Size, Test_Size, X'Length (2));
   begin
      Assert (Natural (Y'Length) = Num_Samples, Routine_Name &
                "Y length" & Integer'Image (Integer (Y'Length)) &
                " is different to X length" & Natural'Image (Num_Samples));

      Init_Base_Shuffle_Split (Shuffle_Data, 1, Train_Size, Test_Size);
      --  L2425
      Base_Shuffle_Split (Shuffle_Data, Train_Indices, Test_Indices);

      for row in Test_X'Range loop
         for col in Test_X'Range (2) loop
            Test_X (row, col) := X (Test_Indices (row), col);
         end loop;

         for col in Test_Y'Range (2) loop
            Test_Y (row, col) := Y (Test_Indices (row), col);
         end loop;
      end loop;

      for row in Train_X'Range loop
         for col in Train_X'Range (2) loop
            Train_X (row, col) :=  X (Train_Indices (row), col);
         end loop;

         for col in Train_Y'Range (2) loop
            Train_Y (row, col) := Y (Train_Indices (row), col);
         end loop;
      end loop;

   end Train_Test_Split;

   --  -------------------------------------------------------------------------
   --  L2325  Boolean Y
   procedure Train_Test_Split
     (X                     : Real_Float_Matrix; Y : Boolean_Matrix;
      Train_Size, Test_Size : Natural;
      Train_X               : out Real_Float_Matrix; Train_Y : out Boolean_Matrix;
      Test_X                : out Real_Float_Matrix; Test_Y : out Boolean_Matrix) is
      Routine_Name      : constant String :=
                            "Data_Splitter.Train_Test_Split Boolean ";
      Num_Samples       : constant Positive := Positive (X'Length);
      Train_Indices     : Integer_Array (1 .. Train_Size);
      Test_Indices      : Integer_Array (1 .. Test_Size);
      Shuffle_Data      : Base_Shuffle_Data
        (Train_Size, Test_Size, X'Length (2));
   begin
      Assert (Natural (Y'Length) = Num_Samples, Routine_Name &
                "Y length" & Integer'Image (Integer (Y'Length)) &
                " is different to X length" & Natural'Image (Num_Samples));

      Init_Base_Shuffle_Split (Shuffle_Data, 1, Train_Size, Test_Size);
      Put_Line (Routine_Name);
      Base_Shuffle_Split (Shuffle_Data, Train_Indices, Test_Indices);

      for row in Test_Indices'Range loop
         for col in Test_X'Range (2) loop
            Test_X (row, col) := X (Test_Indices (row), col);
         end loop;

         for col in Test_Y'Range (2) loop
            Test_Y (row, col) := Y (Test_Indices (row), col);
         end loop;
      end loop;

      for row in Train_X'Range loop
         for col in Train_X'Range (2) loop
            Train_X (row, col) := X (Train_Indices (row), col);
         end loop;

         for col in Train_Y'Range (2) loop
            Train_Y (row, col) := Y (Train_Indices (row), col);
         end loop;
      end loop;

   end Train_Test_Split;

   --  -------------------------------------------------------------------------------

end Data_Splitter;
