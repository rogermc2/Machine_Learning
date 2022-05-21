
--  with Ada.Containers;
with Ada.Text_IO; use Ada.Text_IO;

with Base_Neural;
with Multilayer_Perceptron;
with NL_Arrays_And_Matrices; use NL_Arrays_And_Matrices;
with NL_Types;
--  with Printing;
with Stochastic_Optimizers;

procedure Test_Fit is
--      use Ada.Containers;
    use Multilayer_Perceptron;
    Routine_Name : constant String := "Test_Fit ";
    X            : Real_Float_Matrix (1 .. 1, 1 .. 3);
    Y            : Integer_Matrix (1 .. 1, 1 .. 1);
    Layer_Sizes  : NL_Types.Integer_List;
    aClassifier  : Multilayer_Perceptron.MLP_Classifier;
begin
    Put_Line (Routine_Name);
    X (1, 1) := 0.6;
    X (1, 2) := 0.8;
    X (1, 3) := 0.7;
    Y (1, 1) := 0;
    Layer_Sizes.Append (2);
    aClassifier := C_Init
      (Solver => Stochastic_Optimizers.Sgd_Solver, Learning_Rate_Init => 0.1,
       Alpha => 0.1, Activation => Base_Neural.Logistic_Activation, Random_State => 1,
       Max_Iter => 1, Hidden_Layer_Sizes => Layer_Sizes, Momentum => 0.0);
    --  The Fit function adjusts weights according to data values so
    --  that better accuracy can be achieved
    Fit (aClassifier, X, Y);

end Test_Fit;
