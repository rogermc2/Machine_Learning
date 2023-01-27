
--  with Ada.Directories;
with Ada.Text_IO; use Ada.Text_IO;

with Base;
with Base_Neural;
with CSV_Data_Loader;
with ML_Types;
with Multilayer_Perceptron;
with ML_Arrays_And_Matrices; use ML_Arrays_And_Matrices;
with Test_Support;

procedure Lesson_4B_Neuron is
   use CSV_Data_Loader;
   use Multilayer_Perceptron;
   Routine_Name  : constant String := "Lesson_4B_Neuron ";
   Dataset_Name  : constant String :=
                     "../../../neural_learning/datasets/mnist_784";
   Test_Size     : constant Positive := 1000;
   Train_Size    : constant Positive := 5000;
   Data          : constant Base_Split_State := Get_Split_State
     (Dataset_Name, Digits_Data, Train_Size, Test_Size,
      Y_Categorized => False, Reload => True);
   Train_X       : constant Real_Float_Matrix := Data.Train_X;
   Train_Y       : constant Integer_Matrix := Data.Train_Y;
   Test_X        : constant Real_Float_Matrix := Data.Test_X;
   Test_Y        : constant Integer_Matrix := Data.Test_Y;
   aClassifier   : Multilayer_Perceptron.MLP_Classifier;
   Hidden_Layers : ML_Types.Integer_List;
begin
   Put_Line (Routine_Name);
   Test_Support.Print_Matrix_Dimensions ("Train X", Train_X);
   Test_Support.Print_Matrix_Dimensions ("Test X", Test_X);
   New_Line;

   for run in 1 .. 20 loop
      Hidden_Layers.Clear;
      Hidden_Layers.Append (run * 10);
      aClassifier := C_Init (Max_Iter => 10000, Layer_Sizes => Hidden_Layers,
                             Activation => Base_Neural.Identity_Activation,
                             Verbose => False);
      Put_Line ("run: " & Integer'Image (run));
      --  The Fit function adjusts weights according to data values so
      --  that better accuracy can be achieved
      Fit (aClassifier, Train_X, Train_Y);
      --  Score uses Predict to compare predicted Y values, based on Test_X,
      --  with actual Test_Y values

      Put_Line
        (Integer'Image (run * 10) & " Score: " &
           Float'Image (Base.Score (Self => aClassifier,
                                    X => Test_X, Y => Test_Y)));
   end loop;

   Put_Line ("----------------------------------------------");
   New_Line;

end Lesson_4B_Neuron;
