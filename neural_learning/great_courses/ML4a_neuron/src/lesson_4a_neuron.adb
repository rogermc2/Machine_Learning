
with Ada.Directories;
with Ada.Text_IO; use Ada.Text_IO;

with Base;
with Base_Neural;
with Multilayer_Perceptron;
with NL_Arrays_And_Matrices; use NL_Arrays_And_Matrices;
with Test_Support; use Test_Support;

with Support_4;

procedure Lesson_4A_Neuron is
   use Real_Float_Arrays;
   use Support_4;
   use Multilayer_Perceptron;
   Routine_Name : constant String := "Lesson_4A_Neuron ";
   Dataset_Name : constant String := "mnist_784";
   Train_Size   : constant Positive := 5000;
   Test_Size    : constant Positive := 1000;
   MLP          : Multilayer_Perceptron.MLP_Classifier;
begin
   Put_Line (Routine_Name & "no hidden layers");
   Ada.Directories.Delete_File ("mnist_784.sta");
   declare
      Data          : constant Base_State :=
                        Get_State (Dataset_Name, Train_Size, Test_Size,
                                   Shuffle => False);
      Train_X       : constant Real_Float_Matrix := Data.Train_X;
      Train_Y       : constant Integer_Matrix := Data.Train_Y;
      Test_X        : constant Real_Float_Matrix := Data.Test_X;
      Test_Y        : constant Integer_Matrix := Data.Test_Y;
      Sample_Weight : Real_Float_Vector (1 .. 0);
   begin
      Print_Matrix_Dimensions ("Train X", Train_X);
      Print_Matrix_Dimensions ("Train Y", Train_Y);
      Print_Matrix_Dimensions ("Test X", Test_X);
      Print_Matrix_Dimensions ("Test Y", Test_Y);
      --  default Hidden_Layer_Sizes is empty list
      MLP := C_Init (Max_Iter => 10000,
                     Activation => Base_Neural.Identity_Activation,
                     Verbose => False, Shuffle => False);

      --  Fit function adjusts weights according to data values so that better
      --  accuracy can be achieved
      Fit (MLP, Train_X, Train_Y);

      Put_Line ("Score: " & Float'Image (Base.Score
                (Self => MLP, X => Test_X, Y => Test_Y,
                 Sample_Weight => Sample_Weight)));
   end;  --  declare

   Put_Line ("----------------------------------------------");
   New_Line;

end Lesson_4A_Neuron;
