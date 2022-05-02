
--  Based on scikit-learn/sklearn/model_selection/_split.py

with Ada.Assertions; use Ada.Assertions;
--  with Ada.Text_IO; use Ada.Text_IO;

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
      Train_Size, Test_Size,
      Default_Test_Size      : Natural) is
   begin
      Self.Num_Splits := Num_Splits;
      Self.Train_Size := Train_Size;
      Self.Test_Size := Test_Size;
      Self.Default_Test_Size := Default_Test_Size;

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

         for train_index in Num_Test + 1 .. Num_Samples loop
            Train_Indices (train_index) := Perms (train_index);
         end loop;
      end loop;

   end Iterate_Indices;

   --  -------------------------------------------------------------------------
   --  L2325
   procedure Train_Test_Split
     (X       : Float_Matrix; Y : Integer_Array;
      Train_Size, Test_Size : Natural;
      Train_X : out Float_Matrix; Train_Y : out Integer_Array;
      Test_X  : out Float_Matrix; Test_Y : out Integer_Array) is
      Routine_Name        : constant String := "Data_Splitter.Train_Test_Split ";
      Num_Samples         : constant Positive := X'Length;
--        Num_Features        : constant Positive := X'Length (2);
--        Default_Test_Size   : constant Positive := Train_Size / 4;
--        Shuffle_Data        : Base_Shuffle_Data
--          (Train_Size, Test_Size, Num_Features);
--        X_Vec               : Float_Matrix (1 .. Num_Samples, 1 .. Num_Features);
--        Y_Vec               : Integer_Array (1 .. Num_Samples);
--        Train_Indices       : Integer_Array (1 .. Train_Y'Length);
--        Test_Indices        : Integer_Array (1 .. Test_Y'Length);
   begin
      Assert (Natural (Y'Length) = Num_Samples, Routine_Name &
                "Y length" & Integer'Image (Integer (Y'Length)) &
                " is different to X length" & Natural'Image (Num_Samples));

--        Init_Base_Shuffle_Split (Shuffle_Data, 1, Default_Test_Size);

--        Put_Line (Routine_Name & "Train_X length" &
--                    Integer'Image (Train_X'Length) &
--                 Integer'Image (Train_X'Length (2)));
--        Put_Line (Routine_Name & "Num_Samples, Num_Features" &
--                    Integer'Image (Num_Samples) & " ," &
--                 Integer'Image (Num_Features));
--        for row in X'Range loop
--           for col in X'Range (2) loop
--              X_Vec (row, col) := X (row, col);
--           end loop;
--           Y_Vec (row) := Y (row);
--        end loop;
--        Put_Line (Routine_Name & "X_Vec length" & Integer'Image (X_Vec'Length));

--        Base_Shuffle_Split (Shuffle_Data, Train_Indices, Test_Indices);
--        Printing.Print_Integer_Array ("Test_Indices", Test_Indices);

      for row in 1 .. Train_Size loop
         for col in Train_X'First (2) .. Train_X'Last (2) loop
            Train_X (row, col) := X (row, col);
         end loop;
         Train_Y (row) := Y (row);
      end loop;

      for row in 1 .. Test_Size loop
         for col in Test_X'First (2) .. Test_X'Last (2) loop
            Test_X (row, col) := X (row + Train_Size, col);
         end loop;
         Test_Y (row) := Y (row + Train_Size );
      end loop;

   end Train_Test_Split;

   --  -------------------------------------------------------------------------
   --  L2325
   procedure Train_Test_Split
     (X       : Float_Matrix; Y : Integer_Matrix;
      Train_Size, Test_Size : Natural;
      Train_X : out Float_Matrix; Train_Y : out Integer_Matrix;
      Test_X  : out Float_Matrix; Test_Y : out Integer_Matrix) is
      Routine_Name      : constant String :=
                              "Data_Splitter.Train_Test_Split Integer ";
      Num_Samples       : constant Positive := Positive (X'Length);
      Default_Test_Size : constant Positive := Train_Size / 4;
      Train_Indices     : Integer_Array (1 .. Train_Size);
      Test_Indices      : Integer_Array (1 .. Test_Size);
      Shuffle_Data      : Base_Shuffle_Data
        (Train_Size, Test_Size, X'Length (2));
   begin
      Assert (Natural (Y'Length) = Num_Samples, Routine_Name &
                "Y length" & Integer'Image (Integer (Y'Length)) &
                " is different to X length" & Natural'Image (Num_Samples));

      Init_Base_Shuffle_Split (Shuffle_Data, 1, Train_Size, Test_Size,
                               Default_Test_Size);

      Base_Shuffle_Split (Shuffle_Data, Train_Indices, Test_Indices);

      for index in Test_Indices'First .. Test_Indices'Last loop
         for col in Test_X'First (2) .. Test_X'Last (2) loop
            Test_X (index - Test_Indices'First + 1, col) :=
              X (Test_Indices (index), col);
            Test_Y (index - Test_Indices'First + 1, col) :=
              Y (Test_Indices (index), col);
         end loop;
      end loop;

      for index in Train_Indices'First .. Train_Indices'Last loop
         for col in Train_X'First (2) .. Train_X'Last (2) loop
            Train_X (index - Test_Indices'First + 1, col) :=
              X (Test_Indices (index), col);
            Train_Y (index - Test_Indices'First + 1, col) :=
              Y (Test_Indices (index), col);
         end loop;
      end loop;

   end Train_Test_Split;

   --  -------------------------------------------------------------------------
   --  L2325
   procedure Train_Test_Split
     (X       : Float_Matrix; Y : Boolean_Matrix;
      Train_Size, Test_Size : Natural;
      Train_X : out Float_Matrix; Train_Y : out Boolean_Matrix;
      Test_X  : out Float_Matrix; Test_Y : out Boolean_Matrix) is
      Routine_Name      : constant String :=
                              "Data_Splitter.Train_Test_Split Boolean ";
      Num_Samples       : constant Positive := Positive (X'Length);
      Default_Test_Size : constant Positive := Train_Size / 4;
      Train_Indices     : Integer_Array (1 .. Train_Size);
      Test_Indices      : Integer_Array (1 .. Test_Size);
      Shuffle_Data      : Base_Shuffle_Data
        (Train_Size, Test_Size, X'Length (2));
   begin
      Assert (Natural (Y'Length) = Num_Samples, Routine_Name &
                "Y length" & Integer'Image (Integer (Y'Length)) &
                " is different to X length" & Natural'Image (Num_Samples));

      Init_Base_Shuffle_Split (Shuffle_Data, 1, Train_Size, Test_Size,
                               Default_Test_Size);

      Base_Shuffle_Split (Shuffle_Data, Train_Indices, Test_Indices);

      for index in Test_Indices'First .. Test_Indices'Last loop
         for col in Test_X'First (2) .. Test_X'Last (2) loop
            Test_X (index - Test_Indices'First + 1, col) :=
              X (Test_Indices (index), col);
            Test_Y (index - Test_Indices'First + 1, col) :=
              Y (Test_Indices (index), col);
         end loop;
      end loop;

      for index in Train_Indices'First .. Train_Indices'Last loop
         for col in Train_X'First (2) .. Train_X'Last (2) loop
            Train_X (index - Test_Indices'First + 1, col) :=
              X (Test_Indices (index), col);
            Train_Y (index - Test_Indices'First + 1, col) :=
              Y (Test_Indices (index), col);
         end loop;
      end loop;

   end Train_Test_Split;

   --  -------------------------------------------------------------------------------

end Data_Splitter;
