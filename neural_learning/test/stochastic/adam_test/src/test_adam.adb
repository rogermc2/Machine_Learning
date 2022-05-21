
--  with Ada.Containers;
with Ada.Text_IO; use Ada.Text_IO;

with Multilayer_Perceptron;
--  with NL_Arrays_And_Matrices; use NL_Arrays_And_Matrices;
with NL_Types;
--  with Printing;
with Stochastic_Optimizers;
with Test_Common;

procedure Test_Adam is
   --      use Ada.Containers;
   use Multilayer_Perceptron;
   Routine_Name : constant String := "Test_Adam ";
   Params       : NL_Types.Integer_Array_List;
   aClassifier  : Multilayer_Perceptron.MLP_Classifier;
begin
   Put_Line (Routine_Name);
   Test_Common.Init;
   Params := Test_Common.Shapes;
   aClassifier := C_Init
     (Solver => Stochastic_Optimizers.Adam_Solver, Learning_Rate => 0.001,
      Beta_1 => 0.9, Beta_2 => 0.995, Epsilon => 10.0 ** (-8));
   Initialize (Self => aClassifier);

end Test_Adam;
