
--  with Ada.Assertions; use Ada.Assertions;
with Ada.Text_IO; use Ada.Text_IO;

with Utilities;

with Base;
with Load_Dataset;
with Multilayer_Perceptron; use Multilayer_Perceptron;
with NL_Arrays_And_Matrices; use NL_Arrays_And_Matrices;
with NL_Types;
with Test_Support; use Test_Support;

procedure Simple_From_Data is
   use Real_Float_Arrays;
   Routine_Name : constant String := "Simple_From_Data ";
   Num_Samples  : constant Integer := 100;
   X_Train      : NL_Types.Raw_Data_Vector :=
                    Utilities.Load_Raw_CSV_Data ("../../x_train.csv");
   X_Test       : Real_Float_Matrix (1 .. Num_Samples - 75, 1 .. 2);
   Y_Train      : Integer_Matrix (1 .. 75, 1 .. 1);
   Y_Test       : Integer_Matrix (1 .. Num_Samples - 75, 1 .. 1);
   Indicies     : Integer_Array (1 .. Num_Samples);
   Layer_Sizes  : NL_Types.Integer_List;
   MLP          : MLP_Classifier;
   Score        : Float;
begin
   Put_Line (Routine_Name);

   Layer_Sizes.Append (5);
   MLP := C_Init (Hidden_Layer_Sizes => Layer_Sizes);
   Fit (MLP, X_Train, Y_Train);

   Score := Base.Score (MLP, X_Test, Y_Test);
   Put_Line ("Model accuracy: " & Float'Image (Score));

   declare
      W0 : constant Real_Float_Matrix :=
             MLP.Attributes.Params.Element (1).Coeff_Gradients;
      b0 : constant Real_Float_Vector :=
             MLP.Attributes.Params.Element (1).Intercept_Grads;
      W1 : constant Real_Float_Matrix :=
             MLP.Attributes.Params.Element (2).Coeff_Gradients;
      b1 : constant Real_Float_Vector :=
             MLP.Attributes.Params.Element (2).Intercept_Grads;
   begin
      Print_Float_Matrix ("X_Test", X_Test, 1, 1);
--        Print_Matrix_Dimensions ("Hidden layer W0", W0);
      Print_Float_Matrix ("Hidden layer W0", Transpose (W0));
      Print_Float_Vector ("Hidden layer b0", b0);
--        Print_Matrix_Dimensions ("Output layer W1", W1);
      Print_Float_Matrix ("Output layer W1", Transpose (W1));
      Print_Float_Vector ("Output layer b1", b1);
   end;

end Simple_From_Data;
