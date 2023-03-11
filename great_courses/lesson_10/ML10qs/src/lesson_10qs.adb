
with Ada.Text_IO; use Ada.Text_IO;

--  with Base;
with Base_Neural;
with Basic_Printing; use  Basic_Printing;
with CSV_Data_Loader;
with ML_Types;
with Multilayer_Perceptron;
with ML_Arrays_And_Matrices; use ML_Arrays_And_Matrices;

procedure Lesson_10QS is
   use CSV_Data_Loader;
   use Real_Float_Arrays;
   use Multilayer_Perceptron;
   Program_Name : constant String := "Lesson 10QS ";
   Dataset_Name : constant String :=
                    "../../../neural_learning/datasets/mnist_784";
   Train_Size   : constant Positive := 5000;
   Test_Size    : constant Positive := 1000;
   Data         : constant Base_Split_State :=
     Get_Split_State (Dataset_Name, Digits_Data, Train_Size, Test_Size,
                      Y_Categorized => False, Normalize => False,
                      Reload => True);
   Train_X       : constant Real_Float_Matrix := Data.Train_X;
   Train_Y       : constant Integer_Matrix := Data.Train_Y;
   Test_X        : constant Real_Float_Matrix := Data.Test_X;
   Test_Y        : constant Integer_Matrix := Data.Test_Y;
   Layer_Sizes   : ML_Types.Integer_List;
--     Sample_Weight : Real_Float_Vector (1 .. 0);
   MLP           : Multilayer_Perceptron.MLP_Classifier;
begin
   Put_Line (Program_Name & "no hidden layers");
   Print_Float_Matrix ("Train X", Train_X, 21, 21, 120, 140);
   Print_Matrix_Dimensions ("Train X", Train_X);
   Print_Matrix_Dimensions ("Train Y", Train_Y);
   Print_Matrix_Dimensions ("Test X", Test_X);
   Print_Matrix_Dimensions ("Test Y", Test_Y);
   --  default Hidden_Layer_Sizes is empty list
   MLP := C_Init (Layer_Sizes, Max_Iter => 10000,
                  Activation => Base_Neural.Identity_Activation,
                  Verbose => False, Shuffle => True);

   --  Fit function adjusts weights according to data values so that better
   --  accuracy can be achieved
--     Fit (MLP, Train_X, Train_Y);
--
--     Put_Line ("Score: " & Float'Image (Base.Score
--               (Self => MLP, X => Test_X, Y => Test_Y,
--                Sample_Weight => Sample_Weight)));

   Put_Line ("----------------------------------------------");

   Put_Line (Program_Name & "finished.");
   New_Line;

end Lesson_10QS;
