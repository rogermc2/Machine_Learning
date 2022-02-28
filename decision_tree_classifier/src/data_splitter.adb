
--  Based on scikit-learn/sklearn/model_selection/_split.py

with Ada.Assertions; use Ada.Assertions;
--  with Ada.Text_IO; use Ada.Text_IO;

with Utilities;
with ML_Types;

package body Data_Splitter is

   procedure Iterate_Indices
     (Self : in out Base_Shuffle_Data; Num_Samples : Positive;
      Test_Indices, Train_Indices : out ML_Types.Integer_List);

   --  -------------------------------------------------------------------------
   --  L1569 Shuffle_Split generates indices to split data rows into training
   --  and test sets
   procedure Base_Shuffle_Split
     (Self : in out Base_Shuffle_Data; Num_Samples : Positive;
      Test_Indices, Train_Indices : out ML_Types.Integer_List) is
   begin
      Iterate_Indices (Self, Num_Samples, Test_Indices, Train_Indices);

   end Base_Shuffle_Split;

   --  -------------------------------------------------------------------------

   procedure Init_Base_Shuffle_Split
     (Self : in out Base_Shuffle_Data; Num_Splits : Natural;
      Test_Size, Default_Test_Size, Train_Size : Natural) is
   begin
      Self.Num_Splits := Num_Splits;
      Self.Test_Size := Test_Size;
      Self.Default_Test_Size := Default_Test_Size;
      Self.Train_Size := Train_Size;

   end Init_Base_Shuffle_Split;

   --  -------------------------------------------------------------------------
   --  L1706
   procedure Iterate_Indices
     (Self : in out Base_Shuffle_Data; Num_Samples : Positive;
      Test_Indices, Train_Indices : out ML_Types.Integer_List) is
      use ML_Types;
      use Integer_Package;
      Routine_Name  : constant String := "Data_Splitter.Iterate_Indices ";
      Num_Test      : constant Natural := Self.Test_Size;
      Num_Train     : constant Natural := Self.Train_Size;
      Perms         : Integer_List;
   begin
      Assert (Num_Test + Num_Train = Num_Samples, Routine_Name &
             "invalid Num_Test and Num_Train, sum should be Num_Samples");
      for index in 1 .. Num_Samples loop
         Perms.Append (index);
      end loop;

      --  L1716
      for index in 1 .. Self.Num_Splits loop
         Utilities.Permute (Perms);
         for test in 1 .. Num_Test loop
            Test_Indices.Append (Perms.Element (test));
         end loop;
         for train in Num_Test + 1 .. Num_Samples loop
            Train_Indices.Append (Perms.Element (train));
         end loop;
      end loop;

   end Iterate_Indices;

   --  -------------------------------------------------------------------------

   procedure Train_Test_Split
     (X, Y : ML_Types.Value_Data_Lists_2D; Test_Size, Train_Size : Natural;
      Test_X, Test_Y, Train_X, Train_Y : out ML_Types.Value_Data_Lists_2D) is
      use ML_Types;
      use Value_Lists_Data_Package;
      Routine_Name  : constant String := "Data_Splitter.Train_Test_Split ";
      Num_Samples   : constant Positive := Positive (X.Length);
      Shuffle_Data  : Base_Shuffle_Data;
      X_Cursor      : Value_Lists_Data_Package.Cursor := X.First;
      Y_Cursor      : Value_Lists_Data_Package.Cursor := Y.First;
      X_Vec         : Value_Data_Lists_2D;
      Y_Vec         : Value_Data_Lists_2D;
      Test_Indices  : Integer_List;
      Train_Indices : Integer_List;
   begin
      Assert (Natural (Length (Y)) = Num_Samples, Routine_Name &
             "Y length" & Integer'Image (Integer (Length (Y))) &
             " is different to X length" & Natural'Image (Num_Samples));
      Assert (Train_Size + Test_Size = Num_Samples, Routine_Name &
                "Train_Size" & Integer'Image (Train_Size) & " + Test_Size" &
                Integer'Image (Test_Size) &
                " should equal Num_Samples " & Natural'Image (Num_Samples));

      Init_Base_Shuffle_Split (Shuffle_Data, 1, Test_Size, Num_Samples / 4,
                               Train_Size);

      while Has_Element (X_Cursor) loop
            X_Vec.Append (Element (X_Cursor));
            Y_Vec.Append (Element (Y_Cursor));
            Next (X_Cursor);
            Next (Y_Cursor);
      end loop;

      Base_Shuffle_Split (Shuffle_Data, Num_Samples, Test_Indices,
                          Train_Indices);

      for index in Test_Indices.First_Index .. Test_Indices.Last_Index loop
            Test_X.Append (X_Vec.Element (Test_Indices (index)));
            Test_Y.Append (Y_Vec.Element (Test_Indices (index)));
      end loop;

      for index in Train_Indices.First_Index .. Train_Indices.Last_Index loop
            Train_X.Append (X_Vec.Element (Train_Indices (index)));
            Train_Y.Append (Y_Vec.Element (Train_Indices (index)));
      end loop;

   end Train_Test_Split;

   --  -------------------------------------------------------------------------------

--     procedure Validate_Shuffle_Split
--       (Num_Samples, Test_Size, Train_Size : Natural) is
--     begin
--        null;
--     end Validate_Shuffle_Split;

   --  -------------------------------------------------------------------------------

end Data_Splitter;
