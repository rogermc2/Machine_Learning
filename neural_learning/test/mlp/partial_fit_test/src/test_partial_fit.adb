--  Based on scikit-learn/sklearn/neural_network/_multilayer_perceptron.py
--  test_partial_fit_classification

--  with Ada.Assertions; use Ada.Assertions;
with Ada.Text_IO; use Ada.Text_IO;

with Load_Dataset;
with Multilayer_Perceptron;
with NL_Arrays_And_Matrices; use NL_Arrays_And_Matrices;
with Printing;
with Stochastic_Optimizers;

procedure Test_Partial_Fit is
   use Real_Float_Arrays;
   use Multilayer_Perceptron;
   use Stochastic_Optimizers;

   Routine_Name  : constant String := "Test_Partial_Fit ";
   Data          : constant Load_Dataset.Digits_Data_Record :=
                     Load_Dataset.Load_Digits ("../../digits.csv", 3);
   X             : constant Real_Float_Matrix :=
                     To_Real_Float_Matrix (Data.Features);
   Y             : constant Integer_Matrix := To_Integer_Matrix (Data.Target);
   --      Classes      : constant NL_Types.Class_Names_List := Data.Class_Names;
   aClassifier   : MLP_Classifier;
begin
   Put_Line (Routine_Name);
   Printing.Print_Float_Lists_2D (Routine_Name & "Features", Data.Features);
   Printing.Print_Integer_Matrix (Routine_Name & "Y", Y, 1, 3);
   aClassifier := C_Init
     (Solver => Stochastic_Optimizers.Sgd_Solver, Max_Iter => 100,
      Random_State => 1, Tol => 0.0, Alpha => 10.0 ** (-5),
      Learning_Rate_Init => 0.2);

   Init_Optimizer (aClassifier);
   Put_Line (Routine_Name & "Optimizer initialized");
   Fit (aClassifier, X, Y);
   Put_Line (Routine_Name & "aClassifier fitted");
   declare
      Pred1 : constant Binary_Matrix := Predict (aClassifier, X);
   begin
      Printing.Print_Binary_Matrix ("Pred1", Pred1);
   end;

   --  Partial_Fit updates the model with a single iteration over the data.

end Test_Partial_Fit;
