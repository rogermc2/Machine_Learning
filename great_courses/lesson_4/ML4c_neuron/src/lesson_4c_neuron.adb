
with Ada.Text_IO; use Ada.Text_IO;

with Base;
with Base_Neural;
with CSV_Data_Loader;
with ML_Types;
with Multilayer_Perceptron;
with ML_Arrays_And_Matrices; use ML_Arrays_And_Matrices;
with Test_Support;

procedure Lesson_4C_Neuron is
   use CSV_Data_Loader;
   use Multilayer_Perceptron;
   Routine_Name       : constant String := "Lesson_4C_Neuron ";
   Dataset_Name       : constant String :=
                          "../../../neural_learning/datasets/mnist_784";
   Test_Size          : constant Positive := 1000;
   Train_Size         : constant Positive := 5000;
   Data               : constant Base_Split_State := Get_Split_State
     (Dataset_Name, Digits_Data, Train_Size, Test_Size,
      Y_Categorized => False, Reload => True);
   Train_X            : constant Real_Float_Matrix := Data.Train_X;
   Train_Y            : constant Integer_Matrix := Data.Train_Y;
   Test_X             : constant Real_Float_Matrix := Data.Test_X;
   Test_Y             : constant Integer_Matrix := Data.Test_Y;
   Num_Hidden         : constant Positive := 170;
   aClassifier        : Multilayer_Perceptron.MLP_Classifier;
   Hidden_Layer_Sizes : ML_Types.Integer_List;
begin
   Put_Line (Routine_Name);
   Test_Support.Print_Matrix_Dimensions ("Train X", Train_X);
   Test_Support.Print_Matrix_Dimensions ("Test X", Test_X);
   New_Line;

   Hidden_Layer_Sizes.Append (Num_Hidden);
   for index in 1 .. 10 loop
      aClassifier := C_Init (Max_Iter => 1000, Tol => 0.001,
                             Layer_Sizes => Hidden_Layer_Sizes,
                             Activation => Base_Neural.Identity_Activation,
                             Verbose => False);
      --  The Fit function adjusts weights according to data values so
      --  that better accuracy can be achieved
      Fit (aClassifier, Train_X, Train_Y);
      Put_Line (" Score: " & Float'Image
                (Base.Score (Self => aClassifier,
                             X => Test_X, Y =>Test_Y)));
   end loop;

   Put_Line ("----------------------------------------------");
   New_Line;

end Lesson_4C_Neuron;
