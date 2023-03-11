
with Ada.Text_IO; use Ada.Text_IO;

with CSV_Data_Loader;
with ML_Arrays_And_Matrices; use ML_Arrays_And_Matrices;
with Python;
with Test_Support; use Test_Support;

procedure Lesson_4BP_Neuron is
   use CSV_Data_Loader;
   use Real_Float_Arrays;
   Program_Name : constant String := "Lesson_4BP_Neuron ";
   Dataset_Name : constant String :=
                    "../../../neural_learning/datasets/mnist_784";
   Train_Size   : constant Positive := 5000;
   Test_Size    : constant Positive := 1000;
   Data         : constant Base_Split_State := Get_Split_State
     (Dataset_Name, Digits_Data, Train_Size, Test_Size,
      Y_Categorized => False, Normalize => True, Reload => True);
   Train_X      : constant Real_Float_Matrix := Data.Train_X;
   Train_Y      : constant Integer_Matrix := Data.Train_Y;
   Test_X       : constant Real_Float_Matrix := Data.Test_X;
   Test_Y       : constant Integer_Matrix := Data.Test_Y;
   Train_Y_1D   : Integer_Array (Train_Y'Range);
   Test_Y_1D    : Integer_Array (Test_Y'Range);
   MLP          : Python.Module;
begin
   Put_Line (Program_Name);
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

   Put_Line ("----------------------------------------------");
   New_Line;

end Lesson_4BP_Neuron;
