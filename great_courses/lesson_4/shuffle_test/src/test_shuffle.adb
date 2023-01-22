
with Ada.Directories;
with Ada.Text_IO; use Ada.Text_IO;

with NL_Arrays_And_Matrices; use NL_Arrays_And_Matrices;
with Test_Support; use Test_Support;

with Shuffler; use Shuffler;
with Support_4;

procedure Test_Shuffle is
   use Real_Float_Arrays;
   use Support_4;
   Routine_Name : constant String := "Test_Shuffle ";
   Dataset_Name : constant String := "mnist_784";
   Train_Size   : constant Positive := 4;
   Test_Size    : constant Positive := 1;
begin
   Put_Line (Routine_Name);
   if Ada.Directories.Exists (Dataset_Name & ".sta") then
         Ada.Directories.Delete_File (Dataset_Name & ".sta");
   end if;

   declare
      Data          : constant Base_State :=
                        Get_State (Dataset_Name, Train_Size, Test_Size,
                                   Shuffle => False);
      X          : constant Real_Float_Matrix := Data.Train_X;
      Y          : constant Integer_Matrix := Data.Train_Y;
      Shuffled_X : Real_Float_Matrix := X;
      Shuffled_Y : Integer_Matrix := Y;
   begin
      Put_Line ("Shuffling");
      Shuffle (Shuffled_X, Shuffled_Y);
--        Print_Matrix_Dimensions ("X", X);
--        Print_Matrix_Dimensions ("Y", Y);
--        Print_Matrix_Dimensions ("Shuffled X", Shuffled_X);
--        Print_Matrix_Dimensions ("Shuffled Y", Shuffled_Y);
      Print_Float_Matrix ("X", X, 1, Train_Size, 155, 160);
      Print_Float_Matrix ("Shuffled_X", Shuffled_X, 1, Train_Size, 155, 160);
      Print_Integer_Matrix ("Y", Y);
      Print_Integer_Matrix ("Shuffled_Y", Shuffled_Y);
   end;  --  declare

   Put_Line ("----------------------------------------------");
   New_Line;

end Test_Shuffle;
