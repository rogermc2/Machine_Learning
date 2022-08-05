--  Based on scikit-learn/sklearn/neural_network/_multilayer_perceptron.py
--  test_partial_fit_classification

--  with Ada.Assertions; use Ada.Assertions;
with Ada.Text_IO; use Ada.Text_IO;

with Load_Dataset;
with Multilayer_Perceptron;
with NL_Arrays_And_Matrices; use NL_Arrays_And_Matrices;
with NL_Types;
with Printing;
with Stochastic_Optimizers;
with Utilities;

procedure Test_Partial_Fit is
   use Real_Float_Arrays;
   use Multilayer_Perceptron;
   use Stochastic_Optimizers;

   function Get_CSV_Data (File_Name : String) return NL_Types.Raw_Data_Vector is
      Data_File    : File_Type;
      Raw_CSV_Data : NL_Types.Raw_Data_Vector;
   begin
      Open (Data_File, In_File, File_Name);
      Raw_CSV_Data := Utilities.Load_Raw_CSV_Data (Data_File);
      Close (Data_File);

      for row in Raw_CSV_Data.First_Index .. Raw_CSV_Data.First_Index loop
         Printing.Print_Unbounded_List ("", Raw_CSV_Data.Element (row));
      end loop;

      return Raw_CSV_Data;

   end Get_CSV_Data;

   Routine_Name  : constant String := "Test_Partial_Fit ";
   Raw_CSV_Data  : NL_Types.Raw_Data_Vector :=
                     Get_CSV_Data ("../../digits.csv");
   Data          : constant Load_Dataset.Digits_Data_Record :=
                     Load_Dataset.Load_Digits
                       ("../../digits.csv", Num_Classes => 3);
   X             : constant Real_Float_Matrix :=
                     To_Real_Float_Matrix (Data.Features);
   Y1            : constant Integer_Array := Data.Target;
   Y             : Integer_Matrix (Y1'Range, 1 .. 1);
   --      Classes      : constant Integer_Array := Data.Classes;
   aClassifier   : MLP_Classifier;
begin
   Put_Line (Routine_Name);
--     Printing.Print_Float_Matrix (Routine_Name & "X", X, 1, 3);
   Printing.Print_Integer_Array (Routine_Name & "Y1", Y1, 1, 10);
   for row in Y1'Range loop
      Y (row, 1) := Y1 (row);
   end loop;
   Printing.Print_Integer_Matrix (Routine_Name & "Y", Y, 1, 10);

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
