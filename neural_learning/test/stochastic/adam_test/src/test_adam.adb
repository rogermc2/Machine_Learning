
--  Based on scikit-learn/sklearn/neural_network/tests/
--  test_stochastic_optimizers.py

--  with Ada.Containers;
with Ada.Text_IO; use Ada.Text_IO;

with Multilayer_Perceptron;
--  with NL_Arrays_And_Matrices; use NL_Arrays_And_Matrices;
with NL_Types;
--  with Printing;
with Stochastic_Optimizers;
with Test_Common;

--  Based on  test_adam_optimizer()
procedure Test_Adam is
   --      use Ada.Containers;
   use Multilayer_Perceptron;
   use Stochastic_Optimizers;
   Routine_Name  : constant String := "Test_Adam ";
   Params        : NL_Types.Integer_Array_List;
   Adam          : Stochastic_Optimizers.Adam_Optimizer;
   aClassifier   : MLP_Classifier;
   First_Moment  : Moments_List;
   Second_Moment : Moments_List;
   T             : Natural := 10;
begin
   Put_Line (Routine_Name);
   Test_Common.Init;
   Params := Test_Common.Shapes;

   aClassifier := C_Init
     (Solver => Stochastic_Optimizers.Adam_Solver, Learning_Rate => 0.001,
      Beta_1 => 0.9, Beta_2 => 0.995, Epsilon => 10.0 ** (-8));
    Init_Optimizer (aClassifier);
    Adam.First_Moments := First_Moment;
    Adam.Second_Moments := Second_Moment;
    Adam.Time_Step := T - 1;

end Test_Adam;
