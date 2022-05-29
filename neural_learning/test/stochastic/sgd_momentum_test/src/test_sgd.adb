
--  Based on scikit-learn/sklearn/neural_network/tests/
--  test_stochastic_optimizers.py

--  with Ada.Assertions; use  Ada.Assertions;
with Ada.Text_IO; use Ada.Text_IO;

with Maths;

with NL_Arrays_And_Matrices; use NL_Arrays_And_Matrices;
with Printing;
with Stochastic_Optimizers;
with Test_Common;

--  Based on  test_sgd_optimizer_momentum()
procedure Test_SGD is
   use Real_Float_Arrays;
   use Maths.Float_Math_Functions;
   use Stochastic_Optimizers;
   use Test_Common;
   Routine_Name   : constant String := "Test_SGD ";
   Beta_1         : constant Real_Float_Vector (1 .. 3) :=  (0.9, 1.0, 0.05);
   Beta_2         : constant Real_Float_Vector (1 .. 3) := (0.995, 1.0, 0.001);
   Learning_Rate  : constant Float := 0.1;
   Params         : Parameters_List;
   SGD            : Stochastic_Optimizers.SGD_Optimizer;
   Grads          : Parameters_List;
   Updates        : Parameters_List;
   Expected       : Parameters_List;
   Momentum       : Float;
begin
   Put_Line (Routine_Name);
   Test_Common.Init;
   for shape in Shapes.First_Index .. Shapes.Last_Index loop
      declare
         Bounds    : constant Integer_Array := Shapes.Element (shape);
         Coeff     : constant
           Real_Float_Matrix (1 .. Bounds (1), 1 .. Bounds (2))
           := (others =>  (others => 0.0));
         Ints      : constant Real_Float_Vector (1 .. Bounds (2)) :=
                       (others => 0.0);
         PR        : Parameters_Record (Bounds (1), Bounds (2));
      begin
         PR.Coeff_Gradients := Coeff;
         PR.Intercept_Grads := Ints;
         Params.Append (PR);
      end;
   end loop;

   for mom in 5 .. 9 loop
         Momentum := 0.1 * Float (mom);
         C_Init (Self => SGD, Params => Params,
                 Initial_Learning_Rate => Learning_Rate, Momentum => Momentum);
         for shape in Shapes.First_Index .. Shapes.Last_Index loop
            declare
               Bounds     : constant Integer_Array := Shapes.Element (shape);
               Vel        : Parameters_Record (Bounds (1), Bounds (2));
               GR         : Parameters_Record (Bounds (1), Bounds (2));
            begin
               for row in Vel.Coeff_Gradients'Range loop
                  for col in Vel.Coeff_Gradients'Range (2) loop
                     Vel.Coeff_Gradients (row, col) :=
                          abs (Maths.Random_Float);
                     GR.Coeff_Gradients (row, col) := abs (Maths.Random_Float);
                  end loop;
               end loop;

               for row in Vel.Intercept_Grads'Range loop
                  Vel.Intercept_Grads (row) := abs (Maths.Random_Float);
                  GR.Intercept_Grads (row) := abs (Maths.Random_Float);
               end loop;
               SGD.Velocities.Append (Vel);
               Grads.Append (GR);
            end;
         end loop;

         for index in First_Moments.First_Index .. First_Moments.Last_Index loop
            declare
               FM      : Parameters_Record := First_Moments (index);
               SM      : constant Parameters_Record := Second_Moments (index);
               SM_Sqrt : constant Parameters_Record := Sqrt (SM);
            begin
               FM.Coeff_Gradients := - Learning_Rate * FM.Coeff_Gradients /
                 (SM_Sqrt.Coeff_Gradients);
               FM.Intercept_Grads := - Learning_Rate * FM.Intercept_Grads /
                 SM_Sqrt.Intercept_Grads;
               Updates.Append (FM);
            end;
         end loop;

         Expected := Params + Updates;
         Update_Params (Adam, Grads, Params);
         Printing.Print_Float_Matrix ("Expected Coeff_Gradients", Expected (1).Coeff_Gradients);
         Printing.Print_Float_Array ("Expected Intercept_Grads", Expected (1).Intercept_Grads);

         Printing.Print_Float_Matrix ("Params Coeff_Gradients", Params (1).Coeff_Gradients);
         Printing.Print_Float_Array ("Params Intercept_Grads", Params (1).Intercept_Grads);

         --     for index in Params.First_Index .. Params.Last_Index loop
         --        Assert (Params (index) = Expected (index), Routine_Name &
         --                  "Params" & Integer'Image (index) & "," &
         --                  Float'Image (Params.Element (index).Intercept_Grads (1)) &
         --                  " does not equal expected value" & Integer'Image (index) & ","
         --                & Float'Image (Expected.Element (index).Intercept_Grads (1)));
         --     end loop;
   end loop;

end Test_SGD;
