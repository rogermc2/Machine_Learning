
--  Based on scikit-learn/sklearn/neural_network/tests/
--  test_stochastic_optimizers.py

--  with Ada.Containers;
with Ada.Text_IO; use Ada.Text_IO;

with Maths;

with NL_Arrays_And_Matrices; use NL_Arrays_And_Matrices;
with NL_Types;
--  with Printing;
with Stochastic_Optimizers;
with Test_Common;

--  Based on  test_adam_optimizer()
procedure Test_Adam is
   --      use Ada.Containers;
   use Stochastic_Optimizers;
   use Test_Common;
   Routine_Name   : constant String := "Test_Adam ";
--     Rng           : constant Float := abs (Maths.Random_Float);
   Params         : Parameters_List;
   Adam           : Stochastic_Optimizers.Adam_Optimizer;
   First_Moments  : Moments_List;
   Second_Moments : Moments_List;
   Grads          : Parameters_List;
   T              : Natural := 10;
begin
   Put_Line (Routine_Name);
   Test_Common.Init;
   for index in Shapes.First_Index .. Shapes.Last_Index loop
      declare
         Bounds    : constant NL_Types.Integer_Array := Shapes.Element (index);
         Coeff     : constant Real_Float_Matrix (1 .. Bounds (1), 1 .. Bounds (2))
           := (others =>  (others => 0.0));
         Ints      : constant Real_Float_Vector (1 .. Bounds (1)) :=
                       (others => 0.0);
         PR        : Parameters_Record (Bounds (1), Bounds (2));
         First_Mom : Parameters_Record (Bounds (1), Bounds (2));
         Sec_Mom   : Parameters_Record (Bounds (1), Bounds (2));
         GR        : Parameters_Record (Bounds (1), Bounds (2));
      begin
         PR.Coeff_Grads := Coeff;
         PR.Intercept_Grads := Ints;
         Params.Append (PR);
         for row in First_Mom.Coeff_Grads'Range loop
            for col in First_Mom.Coeff_Grads'Range (2) loop
               First_Mom.Coeff_Grads (row, col) := abs (Maths.Random_Float);
               Sec_Mom.Coeff_Grads (row, col) := abs (Maths.Random_Float);
               GR.Coeff_Grads (row, col) := abs (Maths.Random_Float);
            end loop;
         end loop;

         for row in First_Mom.Intercept_Grads'Range loop
            First_Mom.Intercept_Grads (row) := abs (Maths.Random_Float);
            Sec_Mom.Intercept_Grads (row) := abs (Maths.Random_Float);
            GR.Intercept_Grads (row) := abs (Maths.Random_Float);
         end loop;
         First_Moments.Append (First_Mom);
         Second_Moments.Append (Sec_Mom);
         Grads.Append (GR);
      end;
   end loop;

   C_Init (Self => Adam, Params => Params, Initial_Learning_Rate => 0.001,
           Beta_1 => 0.9, Beta_2 => 0.995, Epsilon => 10.0 ** (-8));
   Adam.First_Moments := First_Moments;
   Adam.Second_Moments := Second_Moments;
   Adam.Time_Step := T - 1;

end Test_Adam;
