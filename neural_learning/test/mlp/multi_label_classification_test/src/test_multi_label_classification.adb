
--  with Ada.Assertions; use Ada.Assertions;
with Ada.Text_IO; use Ada.Text_IO;

with Base_Neural;
with Multilayer_Perceptron; use Multilayer_Perceptron;
with NL_Arrays_And_Matrices; use NL_Arrays_And_Matrices;
with NL_Types;
--  with Printing;
with Samples_Generator; use Samples_Generator;
with Stochastic_Optimizers; use Stochastic_Optimizers;

procedure Test_Multi_Label_Classification is
--      use Real_Float_Arrays;

    Routine_Name   : constant String := "Test_Multi_Label_Classification ";
    Classification : constant Multilabel_Classification :=
                       Make_Multilabel_Classification (N_Samples => 50);
    X              : constant Real_Float_Matrix := Classification.X;
    Y              : constant Integer_Matrix := Classification.Y;
    Y_Bin          : Boolean_Matrix (Y'Range, Y'Range (2));
    Classes        : NL_Types.Integer_List;
    Layer_Sizes    : NL_Types.Integer_List;
    aClassifier    : MLP_Classifier;
begin
    Put_Line (Routine_Name);
    Layer_Sizes.Append (50);
    aClassifier := Multilayer_Perceptron.C_Init
      (Solver => Sgd_Solver, Hidden_Layer_Sizes => Layer_Sizes, Max_Iter => 150,
       Random_State => 0, Activation => Base_Neural.Logistic_Activation,
       Alpha => 10.0 ** (-5), Learning_Rate_Init => 0.2);

    for count in 0 .. 4 loop
        Classes.Append (count);
    end loop;

    for i in 1 .. 100 loop
        Partial_Fit (aClassifier, X, Y, Y_Bin, Classes);
    end loop;

end Test_Multi_Label_Classification;
