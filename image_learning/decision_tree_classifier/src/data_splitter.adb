
--  Based on scikit-learn/sklearn/model_selection/_split.py

with Ada.Assertions; use Ada.Assertions;
with Ada.Text_IO; use Ada.Text_IO;

with Utilities;
with IL_Types;
with Printing;

package body Data_Splitter is

   procedure Iterate_Indices
     (Self                        : in out Base_Shuffle_Data;
      Train_Indices, Test_Indices : out IL_Types.Integer_List);

   --  -------------------------------------------------------------------------
   --  L1569 Shuffle_Split generates indices to split data rows into training
   --  and test sets
   procedure Base_Shuffle_Split
     (Self                        : in out Base_Shuffle_Data;
      Train_Indices, Test_Indices : out IL_Types.Integer_List) is
   begin
      Iterate_Indices (Self, Train_Indices, Test_Indices);

   end Base_Shuffle_Split;

   --  -------------------------------------------------------------------------

   procedure Init_Base_Shuffle_Split
     (Self                                     : in out Base_Shuffle_Data; Num_Splits : Natural;
      Train_Size, Test_Size, Default_Test_Size : Natural) is
   begin
      Self.Num_Splits := Num_Splits;
      Self.Train_Size := Train_Size;
      Self.Test_Size := Test_Size;
      Self.Default_Test_Size := Default_Test_Size;

   end Init_Base_Shuffle_Split;

   --  -------------------------------------------------------------------------
   --  L1706
   procedure Iterate_Indices
     (Self                        : in out Base_Shuffle_Data;
      Train_Indices, Test_Indices : out IL_Types.Integer_List) is
      use IL_Types;
      use Integer_Package;
      --        Routine_Name  : constant String := "Data_Splitter.Iterate_Indices ";
      Num_Train     : constant Natural := Self.Train_Size;
      Num_Test      : constant Natural := Self.Test_Size;
      Num_Samples   : constant Natural := Num_Train + Num_Test;
      Perms         : Integer_List;
   begin
      for index in 1 .. Num_Samples loop
         Perms.Append (index);
      end loop;

      --  L1716
      for index in 1 .. Self.Num_Splits loop
         Utilities.Permute (Perms);
         for test_index in 1 .. Num_Test loop
            Test_Indices.Append (Perms.Element (test_index));
         end loop;

         for train_index in Num_Test + 1 .. Num_Samples loop
            Train_Indices.Append (Perms.Element (train_index));
         end loop;
      end loop;

   end Iterate_Indices;

   --  -------------------------------------------------------------------------

   procedure Train_Test_Split
     (X, Y                             : IL_Types.Value_Data_Lists_2D; Train_Size, Test_Size : Natural;
      Train_X, Train_Y, Test_X, Test_Y : out IL_Types.Value_Data_Lists_2D) is
      use IL_Types;
      use Value_Lists_Data_Package;
      Routine_Name      : constant String := "Data_Splitter.Train_Test_Split ";
      Num_Samples       : constant Positive := Positive (X.Length);
      Default_Test_Size : constant Positive := Train_Size / 4;
      Shuffle_Data      : Base_Shuffle_Data;
      X_Cursor          : Value_Lists_Data_Package.Cursor := X.First;
      Y_Cursor          : Value_Lists_Data_Package.Cursor := Y.First;
      X_Vec             : Value_Data_Lists_2D;
      Y_Vec             : Value_Data_Lists_2D;
      Train_Indices     : Integer_List;
      Test_Indices      : Integer_List;
   begin
      Assert (Natural (Length (Y)) = Num_Samples, Routine_Name &
                "Y length" & Integer'Image (Integer (Length (Y))) &
                " is different to X length" & Natural'Image (Num_Samples));

      Init_Base_Shuffle_Split (Shuffle_Data, 1, Train_Size, Test_Size,
                               Default_Test_Size);

      while Has_Element (X_Cursor) loop
         X_Vec.Append (Element (X_Cursor));
         Y_Vec.Append (Element (Y_Cursor));
         Next (X_Cursor);
         Next (Y_Cursor);
      end loop;
      Put_Line (Routine_Name & "X_Vec length" &
                  Integer'Image (Integer (X_Vec.Length)));

      Base_Shuffle_Split (Shuffle_Data, Train_Indices, Test_Indices);
      Put_Line (Routine_Name & "Base_Shuffle_Split done");
      Printing.Print_Integer_List ("Test_Indices", Test_Indices);

      for index in Test_Indices.First_Index .. Test_Indices.Last_Index loop
         Test_X.Append (X_Vec.Element (Test_Indices (index)));
         Test_Y.Append (Y_Vec.Element (Test_Indices (index)));
      end loop;
      Put_Line (Routine_Name & "Test_Indices done");

      for index in Train_Indices.First_Index .. Train_Indices.Last_Index loop
         Train_X.Append (X_Vec.Element (Train_Indices (index)));
         Train_Y.Append (Y_Vec.Element (Train_Indices (index)));
      end loop;
      Put_Line (Routine_Name & "done");

   end Train_Test_Split;

   --  -------------------------------------------------------------------------------

end Data_Splitter;
