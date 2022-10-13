
with Ada.Directories;
with Ada.Text_IO; use Ada.Text_IO;

with NL_Arrays_And_Matrices; use NL_Arrays_And_Matrices;
with Test_Support; use Test_Support;

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
      Shuffled_Data : constant Base_State :=
                        Get_State (Dataset_Name, Train_Size, Test_Size,
                                   Shuffle => True);
      Train_X       : constant Real_Float_Matrix := Data.Train_X;
      Train_Y       : constant Integer_Matrix := Data.Train_Y;
      Test_X        : constant Real_Float_Matrix := Data.Test_X;
      Test_Y        : constant Integer_Matrix := Data.Test_Y;
   begin
      Print_Matrix_Dimensions ("Train X", Train_X);
      Print_Matrix_Dimensions ("Train Y", Train_Y);
      Print_Matrix_Dimensions ("Test X", Test_X);
      Print_Matrix_Dimensions ("Test Y", Test_Y);
   end;  --  declare

   Put_Line ("----------------------------------------------");
   New_Line;

end Test_Shuffle;
