
with Ada.Text_IO; use Ada.Text_IO;

with Base_Neural;
with Load_Dataset;
with Multilayer_Perceptron;
with NL_Arrays_And_Matrices; use NL_Arrays_And_Matrices;
with NL_Types;
with Printing;
with Stochastic_Optimizers;

procedure Test_Alpha is
   use Real_Float_Arrays;
   use Multilayer_Perceptron;
   use Stochastic_Optimizers;

   Routine_Name  : constant String := "Test_Alpha ";
   Data          : constant Load_Dataset.Data_Record :=
                     Load_Dataset.Load_Digits;
   X             : Real_Float_Matrix :=
                     To_Real_Float_Matrix (Data.Features);
   Y             : Integer_Array := To_Integer_Array (Data.Target);
   Layer_Sizes   : NL_Types.Integer_List;
   Classes       : NL_Types.Integer_List;
   aClassifier   : MLP_Classifier;
   Params        : Parameters_List;

begin
   Put_Line (Routine_Name);
   aClassifier := C_Init
     (Solver => Stochastic_Optimizers.Sgd_Solver, Learning_Rate_Init => 0.1,
      Alpha => 0.1, Activation => Base_Neural.Logistic_Activation,
      Random_State => 1, Max_Iter => 1, Hidden_Layer_Sizes => Layer_Sizes,
      Momentum => 0.0);

   Init_Optimizer (aClassifier);
   Params := aClassifier.Attributes.Params;

--     Printing.Print_Float_Matrix_Formated ("Coeffs (2) errors", Coeff2_Error, 3);

end Test_Alpha;
