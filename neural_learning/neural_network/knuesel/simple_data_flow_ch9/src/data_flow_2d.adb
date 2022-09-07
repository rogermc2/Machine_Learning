
with Ada.Text_IO; use Ada.Text_IO;

with Maths;

with Base;
with Multilayer_Perceptron;
with NL_Arrays_And_Matrices; use NL_Arrays_And_Matrices;
with NL_Types;
--  with Stochastic_Optimizers;
with Test_Support;
with Utilities;

procedure Data_Flow_2D is
   use Real_Float_Arrays;
   use Multilayer_Perceptron;
--     use Stochastic_Optimizers;

   Routine_Name       : constant String := "Data_Flow_2D ";
   Num_Samples        : constant Integer := 75;
   X                  : Real_Float_Matrix (1 .. 100, 1 .. 2);
   Y                  : Float_Array (1 .. 100) := (1 .. 50 => 0.0,
                                                   51 .. 100 => 1.0);
   X_Train            : Real_Float_Matrix (1 .. Num_Samples, 1 .. 2);
   Y_Train            : Real_Float_Matrix (1 .. Num_Samples, 1 .. 2) :=
                          (others => (others => 0.0));
   X_Test             : Real_Float_Matrix (1 .. Num_Samples, 1 .. 2);
   Y_Test             : Real_Float_Matrix (1 .. Num_Samples, 1 .. 2) :=
                          (others => (others => 0.0));
   Layer_Sizes        : NL_Types.Integer_List;
   aClassifier        : MLP_Classifier;
   theScore           : Float;
begin
   Put_Line (Routine_Name);
   for row in 1 .. 50 loop
      X (row, 1) := Maths.Random_Float - 0.3;
      X (row, 2) := Maths.Random_Float + 0.3;
   end loop;

   for row in 51 .. 100 loop
      X (row, 1) := Maths.Random_Float + 0.3;
      X (row, 2) := Maths.Random_Float - 0.3;
   end loop;

   X := Utilities.Permute (X);
   Utilities.Permute (Y);

   for row in 1 .. Num_Samples loop
      Random_Index := Indices (row);
      for col in 1 .. Data.Num_Features loop
         X_Train (row, col) := Features (Random_Index, col);
         X_Test (row, col) := Features (Random_Index, col);
      end loop;
      Y_Train (row, 1) := Data.Target (Random_Index);
      Y_Test (row, 1) := Data.Target (Random_Index);
   end loop;
   Test_Support.Print_Matrix_Dimensions ("X", X_Train);
   Test_Support.Print_Matrix_Dimensions ("Y", Y_Train);

   Layer_Sizes.Append (128);
   aClassifier := C_Init (Hidden_Layer_Sizes => Layer_Sizes);
   Fit (aClassifier, X_Train, Y_Train);

   theScore := Base.Score (aClassifier, X_Test, Y_Test);
   Put_Line ("Score: " & Float'Image (theScore));

end Data_Flow_2D;
