
with Ada.Text_IO; use Ada.Text_IO;

with Base_Neural;
with Multilayer_Perceptron;
with NL_Arrays_And_Matrices; use NL_Arrays_And_Matrices;
with NL_Types;
with Printing;
with Stochastic_Optimizers;

procedure Test_Fit is
    use Real_Float_Arrays;
    use Multilayer_Perceptron;
    use Stochastic_Optimizers;

    Routine_Name : constant String := "Test_Fit ";
    X            : Real_Float_Matrix (1 .. 1, 1 .. 3);
    Y            : Integer_Matrix (1 .. 1, 1 .. 1);
    Layer_Sizes  : NL_Types.Integer_List;
    Classes      : NL_Types.Integer_List;
    aClassifier  : MLP_Classifier;
    Params       : Parameters_List;
    Coeff1_Test  : constant Real_Float_Matrix (1 .. 3, 1 .. 2) :=
                     ((0.098, 0.195756), (0.2956664, 0.096008),
                      (0.4939998, -0.002244));
    Coeff2_Test  : constant Real_Float_Matrix (1 .. 2, 1 .. 1) :=
                     ((1 => 0.04706), (1 => 0.154089));
    Coeff1_Error  : Real_Float_Matrix
      (Coeff1_Test'Range, Coeff1_Test'Range (2));
    Coeff2_Error  : Real_Float_Matrix
      (Coeff2_Test'Range, Coeff2_Test'Range (2));

    procedure Set_Weights (Self : in out MLP_Classifier) is
        Coeffs_1     : constant Real_Float_Matrix (1 .. 3, 1 .. 2) :=
                         ((0.1, 0.2), (0.3, 0.1), (0.5, 0.0));
        Intercepts_1 : constant Real_Float_Vector (1 .. 2) := (0.1, 0.1);
        Coeffs_2     : Real_Float_Matrix (1 .. 2, 1 .. 1);
        Intercepts_2 : Real_Float_Vector (1 .. 1);
        Params_1     : Parameters_Record (3, 2);
        Params_2     : Parameters_Record (2, 1);
    begin
        Coeffs_2 (1, 1) := 0.1;
        Coeffs_2 (2, 1) := 0.1;
        Intercepts_2 (1) := (1.0);
        Params_1.Coeff_Gradients := Coeffs_1;
        Params_1.Intercept_Grads := Intercepts_1;
        Params_2.Coeff_Gradients := Coeffs_2;
        Params_2.Intercept_Grads := Intercepts_2;
        Self.Attributes.Params.Append (Params_1);
        Self.Attributes.Params.Append (Params_2);

    end Set_Weights;

begin
    Put_Line (Routine_Name);
    X (1, 1) := 0.6;
    X (1, 2) := 0.8;
    X (1, 3) := 0.7;
    Y (1, 1) := 0;
    Layer_Sizes.Append (2);
    aClassifier := C_Init
      (Solver => Stochastic_Optimizers.Sgd_Solver, Learning_Rate_Init => 0.1,
       Alpha => 0.1, Activation => Base_Neural.Logistic_Activation,
       Random_State => 1, Max_Iter => 1, Hidden_Layer_Sizes => Layer_Sizes,
       Momentum => 0.0);

    Set_Weights (aClassifier);
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

    --  Partial_Fit updates the model with a single iteration over the data.
    Classes.Append (0);
    Classes.Append (1);
    Partial_Fit (aClassifier, X, Y, Classes => Classes);

    Params := aClassifier.Attributes.Params;
    Coeff1_Error :=
      abs (Coeff1_Test - Params.Element (1).Coeff_Gradients);
    Printing.Print_Float_Matrix_Formated ("Coeffs (1) errors", Coeff1_Error, 3);
    Coeff2_Error :=
      abs (Coeff2_Test - Params.Element (2).Coeff_Gradients);
    Printing.Print_Float_Matrix_Formated ("Coeffs (2) errors", Coeff2_Error, 3);

end Test_Fit;
