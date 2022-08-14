--  Based on scikit-learn/sklearn/neural_network/_multilayer_perceptron.py
--  test_partial_fit_classification

--  with Ada.Assertions; use Ada.Assertions;
with Ada.Text_IO; use Ada.Text_IO;

--  with Base;
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

   --  -------------------------------------------------------------------------

   function Load_Y (Y1 : Integer_Array) return Integer_Matrix is
      YM : Integer_Matrix (Y1'Range, 1 .. 1);
   begin
      for row in Y1'Range loop
         YM (row, 1) := Y1 (row);
      end loop;

      return YM;

   end Load_Y;

   --  -------------------------------------------------------------------------

   Data                : constant Load_Dataset.Digits_Data_Record :=
                           Load_Dataset.Load_Digits
                             ("../../digits.csv", Num_Classes => 3);
   X                   : constant Real_Float_Matrix :=
                           To_Real_Float_Matrix (Data.Features);
   Y                   : constant Integer_Matrix := Load_Y (Data.Target);
   --      Classes      : constant Integer_Array := Data.Classes;
   aClassifier         : MLP_Classifier;
--     Score               : Float;
begin
   Put_Line (Routine_Name & "Y Length" & Integer'Image (Y'Length));
   --  Printing.Print_Float_Matrix (Routine_Name & "X", X, 1, 3);
   --  Printing.Print_Integer_Matrix (Routine_Name & "Y", Y, 1, 10);

   aClassifier := C_Init
     (Solver => Stochastic_Optimizers.Sgd_Solver, Max_Iter => 1000,
      Random_State => 1, Tol => 0.0, Alpha => 10.0 ** (-5),
      Learning_Rate_Init => 0.2, Verbose => False);

   Init_Optimizer (aClassifier);
   Put_Line (Routine_Name & "Optimizer initialized");
   Fit (aClassifier, X, Y);
   Put_Line (Routine_Name & "aClassifier fitted");
   declare
      Pred1 : constant Integer_Matrix := Predict (aClassifier, X);
   begin
      Printing.Print_Integer_Matrix ("Errors Pred1 - Y", Pred1 - Y, 1, 10);
--        Printing.Print_Integer_Matrix ("Pred1", Pred1, 35, 50);
--        Printing.Print_Integer_Matrix ("Y", Y, 35, 50);
   end;

--     Score := Base.Score (aClassifier, X, Y);
--     Put_Line (Routine_Name & " Score:" & Float'Image (Score));

end Test_Partial_Fit;
