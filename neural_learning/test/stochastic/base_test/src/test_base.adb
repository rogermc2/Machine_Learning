
--  Based on scikit-learn/sklearn/neural_network/tests/
--  test_stochastic_optimizers.py

with Ada.Assertions; use  Ada.Assertions;
with Ada.Text_IO; use Ada.Text_IO;

with Stochastic_Optimizers;

procedure Test_Base is
   use Stochastic_Optimizers;
   Routine_Name   : constant String := "Test_Base ";
   Opt            : Base_Optimizer;
   Opt_Record     : Optimizer_Record (Optimizer_Base);
begin
   Put_Line (Routine_Name);
   for lr in -3 .. 4 loop
      C_Init (Opt, 10.0 ** lr);
   end loop;

   Assert (Trigger_Stopping (Opt_Record, "Trigger_Stopping state:", True),
          "Test_Base failed");
   Put_Line ("Test_Base passed");

end Test_Base;
