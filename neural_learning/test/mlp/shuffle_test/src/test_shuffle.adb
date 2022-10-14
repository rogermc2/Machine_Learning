
with Ada.Assertions; use Ada.Assertions;
with Ada.Text_IO; use Ada.Text_IO;

with Multilayer_Perceptron;
with NL_Arrays_And_Matrices; use NL_Arrays_And_Matrices;
with NL_Types;
with Samples_Generator;

procedure Test_Shuffle is
    use Real_Float_Arrays;
    use Multilayer_Perceptron;

    Routine_Name : constant String := "Test_Test_Shuffle ";
    Weights      :  NL_Types.Float_List;
    Data         : constant Samples_Generator.Classification_Test_Data :=
                     Samples_Generator.Make_Classification
--                         (N_Samples => 50, N_Features => 5,
                        (Weights => Weights, Shuffle => False);
    X            : constant Real_Float_Matrix := Data.X;
    Y            : constant Integer_Matrix := To_Integer_Matrix (Data.Y);
    Layer_Sizes  : NL_Types.Integer_List;
    MLP1         : MLP_Classifier;
    MLP2         : MLP_Classifier;

begin
    Put_Line (Routine_Name);
    Layer_Sizes.Append (1);
    MLP1 := C_Init
      (Max_Iter => 1, Hidden_Layer_Sizes => Layer_Sizes,
       Random_State => 0, Shuffle => True);
    MLP2 := C_Init
      (Max_Iter => 1, Hidden_Layer_Sizes => Layer_Sizes,
       Random_State => 0, Shuffle => True);

    Init_Optimizer (MLP1);
    Init_Optimizer (MLP2);

    Fit (MLP1, X, Y);
    Fit (MLP2, X, Y);

   Assert (MLP2.Attributes.Params (1).Coeff_Gradients =
             MLP1.Attributes.Params (1).Coeff_Gradients,
           "Coeffs (1) Test failed");
   Put_Line ("Both true test passed");

    MLP2 := C_Init
      (Max_Iter => 1, Hidden_Layer_Sizes => Layer_Sizes,
       Random_State => 0, Shuffle => False);
   Init_Optimizer (MLP2);

    Fit (MLP1, X, Y);
    Fit (MLP2, X, Y);

   Assert (MLP2.Attributes.Params (1).Coeff_Gradients =
             MLP1.Attributes.Params (1).Coeff_Gradients,
           "True/False test passed");

   Put_Line ("Coeffs tests passed");

end Test_Shuffle;
