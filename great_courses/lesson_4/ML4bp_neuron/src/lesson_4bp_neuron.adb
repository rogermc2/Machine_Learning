
--  with Ada.Directories;
with Ada.Text_IO; use Ada.Text_IO;

with NL_Arrays_And_Matrices; use NL_Arrays_And_Matrices;
with Python;
with Test_Support; use Test_Support;

with Support_4;

procedure Lesson_4BP_Neuron is
   use Real_Float_Arrays;
   use Support_4;
   Routine_Name : constant String := "Lesson_4BP_Neuron ";
   Dataset_Name : constant String := "mnist_784";
   Train_Size   : constant Positive := 5000;
   Test_Size    : constant Positive := 1000;
begin
   Put_Line (Routine_Name);
   --     if Ada.Directories.Exists (Dataset_Name & ".sta") then
   --        Ada.Directories.Delete_File (Dataset_Name & ".sta");
   --     end if;

   declare
      Data       : constant Base_State :=
                     Get_State (Dataset_Name, Train_Size, Test_Size,
                                Shuffle => True);
      Train_X    : constant Real_Float_Matrix := Data.Train_X;
      Train_Y    : constant Integer_Matrix := Data.Train_Y;
      Test_X     : constant Real_Float_Matrix := Data.Test_X;
      Test_Y     : constant Integer_Matrix := Data.Test_Y;
      Train_Y_1D : Integer_Array (Train_Y'Range);
      Test_Y_1D  : Integer_Array (Test_Y'Range);
      MLP        : Python.Module;
   begin
      Print_Matrix_Dimensions ("Train X", Train_X);
      Print_Matrix_Dimensions ("Train Y", Train_Y);
      for index in Train_Y'Range loop
         Train_Y_1D (index) := Train_Y (index, 1);
      end loop;
      for index in Test_Y'Range loop
         Test_Y_1D (index) := Test_Y (index, 1);
      end loop;

      New_Line;
      Python.Initialize;
      MLP := Python.Import_File ("lesson_4bp");

      Python.Call (MLP, "classify",
                   Train_X, Train_Y_1D, Test_X, Test_Y_1D);

      Python.Close_Module (MLP);
      Python.Finalize;
   end;

   Put_Line ("----------------------------------------------");
   New_Line;

end Lesson_4BP_Neuron;
