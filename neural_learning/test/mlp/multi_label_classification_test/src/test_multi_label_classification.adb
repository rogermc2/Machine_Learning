
--  with Ada.Assertions; use Ada.Assertions;
with Ada.Text_IO; use Ada.Text_IO;

with Base_Neural;
with base;
with Multilayer_Perceptron; use Multilayer_Perceptron;
with NL_Arrays_And_Matrices; use NL_Arrays_And_Matrices;
with NL_Types;
--  with Printing;
with Samples_Generator; use Samples_Generator;
with Stochastic_Optimizers; use Stochastic_Optimizers;

procedure Test_Multi_Label_Classification is

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
   Put_Line (Routine_Name & "initializing aClassifier");
   aClassifier := Multilayer_Perceptron.C_Init
     (Solver => Sgd_Solver, Hidden_Layer_Sizes => Layer_Sizes, Max_Iter => 150,
      Random_State => 0, Activation => Base_Neural.Logistic_Activation,
      Alpha => 10.0 ** (-5), Learning_Rate_Init => 0.2);

  Put_Line (Routine_Name & "aClassifier initialized");
   for count in 0 .. 4 loop
      Classes.Append (count);
   end loop;

   for test_num in 1 .. 100 loop
      Put_Line (Routine_Name & "test_num" & Integer'Image (test_num));
      Partial_Fit (aClassifier, X, Y, Y_Bin, Classes);
   end loop;

   Put_Line (Routine_Name & "Score" &
               Float'Image (Base.Score (aClassifier, X, Y)));

end Test_Multi_Label_Classification;
