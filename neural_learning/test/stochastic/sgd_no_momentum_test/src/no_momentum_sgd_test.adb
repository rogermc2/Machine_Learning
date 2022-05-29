
--  Based on scikit-learn/sklearn/neural_network/tests/
--  test_stochastic_optimizers.py

with Ada.Assertions; use  Ada.Assertions;
with Ada.Text_IO; use Ada.Text_IO;

with Maths;

with NL_Arrays_And_Matrices; use NL_Arrays_And_Matrices;
with Printing;
with Stochastic_Optimizers;
with Test_Common;

--  Based on  test_sgd_optimizer_no_momentum()
procedure No_Momentum_SGD_Test is
   use Stochastic_Optimizers;
   use Test_Common;
   Routine_Name   : constant String := "No_Momentum_SGD_Test ";
   Learn_Rate     : Float;
   Params         : Parameters_List;
   SGD            : Stochastic_Optimizers.SGD_Optimizer;
   Grads          : Parameters_List;
   Expected       : Parameters_List;
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

   for lr in -3 .. 4 loop
      Put_Line ("Learning rate:" & Integer'Image (lr));
      Learn_Rate := 10.0 ** lr;
      C_Init (SGD, Params => Params, Learning_Rate => Learn_Rate, Momentum => 0.0);

      for shape in Shapes.First_Index .. Shapes.Last_Index loop
         declare
            use Maths;
            Bounds : constant Integer_Array := Shapes.Element (shape);
            Coeff  : Real_Float_Matrix (1 .. Bounds (1), 1 .. Bounds (2));
            Ints   : Real_Float_Vector (1 .. Bounds (2));
            GR     : Parameters_Record (Bounds (1), Bounds (2));
         begin
            for row in Coeff'Range loop
               for col in Coeff'Range (2) loop
                  Coeff (row, col) := abs (Random_Float);
               end loop;
               GR.Coeff_Gradients := Coeff;
            end loop;

            for col in Ints'Range loop
               Ints (col) := abs (Random_Float);
            end loop;
            GR.Intercept_Grads := Ints;
            Grads.Append (GR);
         end;
      end loop;

      for index in Params.First_Index .. Params.Last_Index loop
         declare
            Par : Parameters_Record := Params (index);
         begin
            Par := Par - Learn_Rate * Grads (index);
            Expected.Append (Par);
         end;
      end loop;

      Update_Params (SGD, Params, Grads);

      Printing.Print_Float_Matrix ("Expected Coeff_Gradients",
                                   Expected (1).Coeff_Gradients);
      Printing.Print_Float_Array ("Expected Intercept_Grads",
                                  Expected (1).Intercept_Grads);

      Printing.Print_Float_Matrix ("Params Coeff_Gradients",
                                   Params (1).Coeff_Gradients);
      Printing.Print_Float_Array ("Params Intercept_Grads",
                                  Params (1).Intercept_Grads);
      New_Line;

      for index in Params.First_Index .. Params.Last_Index loop
         Assert (Params (index) = Expected (index),
                   "Params" & Integer'Image (index) & ", " &
                   Float'Image (Params.Element (index).Intercept_Grads (1)) &
                   " does not equal expected value" & Integer'Image (index) & ","
                 & Float'Image (Expected.Element (index).Intercept_Grads (1)));
      end loop;
   end loop;

end No_Momentum_SGD_Test;
