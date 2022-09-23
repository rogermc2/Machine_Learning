
--  with Ada.Assertions; use Ada.Assertions;
with Ada.Text_IO; use Ada.Text_IO;

with Maths;
with Utilities;

with Base;
with Multilayer_Perceptron; use Multilayer_Perceptron;
with NL_Arrays_And_Matrices; use NL_Arrays_And_Matrices;
with NL_Types;
with Test_Support; use Test_Support;

procedure Simple is
   use Real_Float_Arrays;
   Routine_Name : constant String := "Simple ";
   Num_Samples  : constant Integer := 100;
   X1           : Float_Array (1 .. 50);
   X2           : Float_Array (1 .. 50);
   Y1           : Float_Array (1 .. 50);
   Y2           : Float_Array (1 .. 50);
   X            : Real_Float_Matrix (1 .. Num_Samples, 1 .. 2);
   Y            : Integer_Matrix (1 .. Num_Samples, 1 .. 1) :=
                    (1 .. 50 => (others => 0),
                     51 .. Num_Samples => (others => 1));
   X_Train      : Real_Float_Matrix (1 .. 75, 1 .. 2);
   X_Test       : Real_Float_Matrix (1 .. Num_Samples - 75, 1 .. 2);
   Y_Train      : Integer_Matrix (1 .. 75, 1 .. 1);
   Y_Test       : Integer_Matrix (1 .. Num_Samples - 75, 1 .. 1);
   Layer_Sizes  : NL_Types.Integer_List;
   MLP          : MLP_Classifier;
   Score        : Float;
begin
   Put_Line (Routine_Name);
   for row in X1'Range loop
      X1 (row) := abs (Maths.Random_Float) - 0.3;
      X2 (row) := abs (Maths.Random_Float) + 0.3;
      Y1 (row) := abs (Maths.Random_Float) + 0.3;
      Y2 (row) := abs (Maths.Random_Float) - 0.3;
   end loop;

   for row in 1 ..50 loop
      X (row, 1) := X1 (row);
      X (row, 2) := Y1 (row);
   end loop;
   for row in 51 .. Num_Samples loop
      X (row, 1) := X2 (row - 50);
      X (row, 2) := Y2 (row - 50);
   end loop;

   X := Utilities.Permute (X);
   Y := Utilities.Permute (Y);

   for row in 1 .. 75 loop
      for col in X'Range (2) loop
         X_Train (row, col) := X (row, col);
      end loop;
      Y_Train (row, 1) := Y (row, 1);
   end loop;

   for row in 76 .. Num_Samples loop
      for col in X'Range (2) loop
         X_Test (row - 75, col) := X (row, col);
      end loop;
      Y_Test (row - 75, 1) := Y (row, 1);
   end loop;

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
      Print_Matrix_Dimensions ("W0", W0);
      Print_Float_Matrix ("W0", Transpose (W0));
      Print_Float_Vector ("b0", b0);
      Print_Matrix_Dimensions ("W1", W1);
      Print_Float_Matrix ("W1", Transpose (W1));
      Print_Float_Vector ("b1", b1);
   end;

end Simple;
