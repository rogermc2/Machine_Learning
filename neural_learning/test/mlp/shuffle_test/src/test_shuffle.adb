
with Ada.Assertions; use Ada.Assertions;
with Ada.Text_IO; use Ada.Text_IO;

with Multilayer_Perceptron;
with NL_Arrays_And_Matrices; use NL_Arrays_And_Matrices;
with NL_Types;
with Samples_Generator;
with Test_Support; use Test_Support;

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
    Classifier1  : MLP_Classifier;
    Classifier2  : MLP_Classifier;

    procedure Test (MLP1, MLP2 : out MLP_Classifier;
                    Shuffle1, Shuffle2 : Boolean) is
    begin
        MLP1 := C_Init
          (Max_Iter => 1, Hidden_Layer_Sizes => Layer_Sizes,
           Random_State => 0, Shuffle => Shuffle1);
        MLP2 := C_Init
          (Max_Iter => 1, Hidden_Layer_Sizes => Layer_Sizes,
           Random_State => 0, Shuffle => Shuffle2);

        Init_Optimizer (MLP1);
        Init_Optimizer (MLP2);

        Fit (MLP1, X, Y);
        Fit (MLP2, X, Y);

    end Test;

begin
    Put_Line (Routine_Name);
    Layer_Sizes.Append (1);

    Test (Classifier1, Classifier2, False, False);
    Print_Float_Matrix
      ("Classifier1 Coeff_Gradients",
        Classifier1.Attributes.Params (1).Coeff_Gradients, 1, 5);
    Print_Float_Matrix
      ("Classifier1 Coeff_Gradients",
        Classifier2.Attributes.Params (1).Coeff_Gradients, 1, 5);

    Assert (Classifier2.Attributes.Params (1).Coeff_Gradients =
              Classifier1.Attributes.Params (1).Coeff_Gradients,
            "False, False test failed");
    Put_Line ("Both false test passed");

    Test (Classifier1, Classifier2, True, True);

    Assert (Classifier2.Attributes.Params (1).Coeff_Gradients =
              Classifier1.Attributes.Params (1).Coeff_Gradients,
            "Coeffs (1) Test failed");
    Put_Line ("Both true test passed");

    Test (Classifier1, Classifier2, True, False);

    Assert (Classifier2.Attributes.Params (1).Coeff_Gradients =
              Classifier1.Attributes.Params (1).Coeff_Gradients,
            "True/False test passed");

    Put_Line ("Coeffs tests passed");

end Test_Shuffle;
