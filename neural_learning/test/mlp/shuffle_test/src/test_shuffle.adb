
--  with Ada.Assertions; use Ada.Assertions;
with Ada.Text_IO; use Ada.Text_IO;

with Base_Neural;
with Multilayer_Perceptron;
with NL_Arrays_And_Matrices; use NL_Arrays_And_Matrices;
with NL_Types;
with Samples_Generator;
with Stochastic_Optimizers;

procedure Test_Shuffle is
    use Real_Float_Arrays;
    use Multilayer_Perceptron;
    use Stochastic_Optimizers;

    Routine_Name : constant String := "Test_Test_Shuffle ";
    Weights      :  NL_Types.Float_List;
    Data         : constant Samples_Generator.Classification_Test_Data :=
                     Samples_Generator.Make_Classification
                       (N_Samples => 50, N_Features => 5,
                        Weights => Weights, Shuffle => False);
    X            : Real_Float_Matrix := Data.X;
    Y            : Integer_Matrix := To_Integer_Matrix (Data.Y);
    Layer_Sizes  : NL_Types.Integer_List;
    aClassifier  : MLP_Classifier;

begin
    Put_Line (Routine_Name);
    Layer_Sizes.Append (1);
    aClassifier := C_Init
      (Solver => Stochastic_Optimizers.Sgd_Solver, Learning_Rate_Init => 0.1,
       Alpha => 0.1, Activation => Base_Neural.Logistic_Activation,
       Random_State => 1, Max_Iter => 1, Hidden_Layer_Sizes => Layer_Sizes,
       Momentum => 0.0);

    aClassifier.Attributes.N_Outputs := 1;
    aClassifier.Attributes.N_Features := 3;
    aClassifier.Attributes.N_Iter := 0;
    aClassifier.Parameters.Learning_Rate := 0.1;
    aClassifier.Attributes.N_Layers := 3;
    aClassifier.Attributes.Out_Activation := Base_Neural.Logistic_Activation;
    aClassifier.Attributes.T := 0;
    aClassifier.Attributes.Best_Loss := Float'Last;
    aClassifier.Attributes.No_Improvement_Count := 0;
    aClassifier.Parameters.Solver := Sgd_Solver;

    Init_Optimizer (aClassifier);
    aClassifier.Attributes.Optimizer.SGD.Power_T := 0.0;

    Fit (aClassifier, X, Y);

--      Assert (Coeff1_Test = Params.Element (1).Coeff_Gradients,
--              "Coeffs (1) Test failed");
--      Assert (Coeff2_Test = Params.Element (2).Coeff_Gradients,
--              "Coeffs (2) Test failed");
--      Put_Line ("Coeffs tests passed");

end Test_Shuffle;
