
--  Based on scikit-learn/sklearn/model_selection/_split.py

with Ada.Assertions; use Ada.Assertions;
with Utilities;

package body Data_Splitter is

   procedure Iterate_Indices
     (Self : in out Base_Shuffle_Data; X, Y : ML_Types.String_List;
      Test_Indices, Train_Indices : out ML_Types.Integer_List);

   --  -------------------------------------------------------------------------
   --  Shuffle_Split generates indices to split data into training and test set
   procedure Base_Shuffle_Split
     (Self : in out Base_Shuffle_Data; X, Y : ML_Types.String_List;
      Test_Indices, Train_Indices : out ML_Types.Integer_List) is
--        use ML_Types;
   begin
      Iterate_Indices (Self, X, Y, Test_Indices, Train_Indices);
   end Base_Shuffle_Split;

   --  -------------------------------------------------------------------------

   procedure Base_Shuffle_Split (Self : in out Base_Shuffle_Data;
                                 X, Y : ML_Types.Value_Data_Lists_2D) is
   begin
      null;
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

   procedure Iterate_Indices
     (Self : in out Base_Shuffle_Data; X, Y : ML_Types.String_List;
      Test_Indices, Train_Indices : out ML_Types.Integer_List) is
      use ML_Types;
      use Integer_Package;
      use String_Package;
      Routine_Name  : constant String := "Utilities.Iterate_Indices ";
      Num_Samples   : constant Positive := Positive (Length (X));
      Num_Test      : constant Natural := Self.Test_Size;
      Num_Train     : constant Natural := Self.Train_Size;
      Perms         : Integer_List;
   begin
      Assert (Num_Test + Num_Train = Num_Samples, Routine_Name &
             "invalid Num_Test and Num_Train, sum should be Num_Samples");
      for index in 1 .. Num_Samples loop
         Perms.Append (index);
      end loop;

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

   procedure Train_Test_Split (X, Y : ML_Types.String_List) is
   begin
      null;
   end Train_Test_Split;

   --  -------------------------------------------------------------------------------

   procedure Train_Test_Split (X, Y : ML_Types.Value_Data_Lists_2D) is
   begin
      null;
   end Train_Test_Split;

   --  -------------------------------------------------------------------------------

--     procedure Validate_Shuffle_Split
--       (Num_Samples, Test_Size, Train_Size : Natural) is
--     begin
--        null;
--     end Validate_Shuffle_Split;

   --  -------------------------------------------------------------------------------

end Data_Splitter;
