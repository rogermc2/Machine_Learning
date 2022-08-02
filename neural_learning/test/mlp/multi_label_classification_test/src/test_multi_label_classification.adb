
--  with Ada.Assertions; use Ada.Assertions;
with Ada.Text_IO; use Ada.Text_IO;

with Base;
with Base_Neural;
with Multilayer_Perceptron; use Multilayer_Perceptron;
with NL_Arrays_And_Matrices; use NL_Arrays_And_Matrices;
with NL_Types;
with Printing;
with Samples_Generator; use Samples_Generator;
with Stochastic_Optimizers; use Stochastic_Optimizers;

procedure Test_Multi_Label_Classification is

   Routine_Name        : constant String := "Test_Multi_Label_Classification ";
   Test_Classification : constant Classification_Test_Data :=
                           Make_Multilabel_Classification (N_Samples => 50);
   --  Default Classification_Test_Data parameters:
   --  N_Features: 20;
   --  N_Classes:  5;
   --  N_Labels:   2;
   Test_X              : constant Real_Float_Matrix := Test_Classification.X;
   Test_Y              : constant Binary_Matrix := Test_Classification.Y;
   Classes             : NL_Types.Integer_List;
   Layer_Sizes         : NL_Types.Integer_List;
   aClassifier         : MLP_Classifier;
   Score               : Float;
begin
   Put_Line (Routine_Name);
   Layer_Sizes.Append (50);

   aClassifier := Multilayer_Perceptron.C_Init
     (Solver => Sgd_Solver, Hidden_Layer_Sizes => Layer_Sizes, Max_Iter => 150,
      Random_State => 0, Activation => Base_Neural.Logistic_Activation,
      Alpha => 10.0 ** (-5), Learning_Rate_Init => 0.2, Verbose => False);

   Put_Line (Routine_Name & "Test_X size" & Integer'Image (Test_X'Length)
             & " x" & Integer'Image (Test_X'Length (2)));
   Put_Line (Routine_Name & "Test_Y size" & Integer'Image (Test_Y'Length)
             & " x" & Integer'Image (Test_Y'Length (2)));
   Printing.Print_Binary_Matrix (Routine_Name & "Test_Y", Test_Y, 1, 6);

   --  L393
   for count in 0 .. 4 loop
      Classes.Append (count);
   end loop;
--  OK here Test_Y size 50 x 5
--  --     for test_num in 1 .. 100 loop
   for test_num in 1 .. 1
   loop
      Put_Line (Routine_Name & "test_num" & Integer'Image (test_num));
      Partial_Fit (aClassifier, Test_X, Test_Y, Classes);
--        Score := Base.Score (aClassifier, Test_X, To_Integer_Matrix (Test_Y));
--        Put_Line (Routine_Name & "Score" & Float'Image (Score));
   end loop;

end Test_Multi_Label_Classification;
