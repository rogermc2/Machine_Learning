
--  Based on scikit-learn/sklearn/neural_network/tests/
--  test_stochastic_optimizers.py

with Ada.Assertions; use  Ada.Assertions;
with Ada.Text_IO; use Ada.Text_IO;

with Maths;

with NL_Arrays_And_Matrices; use NL_Arrays_And_Matrices;
--  with Printing;
with Stochastic_Optimizers;
with Test_Common;

--  Based on  test_sgd_optimizer_momentum()
procedure Test_SGD is
--      use Real_Float_Arrays;
    use Stochastic_Optimizers;
    use Test_Common;
    Routine_Name   : constant String := "Test_SGD ";
    Learning_Rate  : constant Float := 0.1;
    Params         : Parameters_List;
    SGD            : Stochastic_Optimizers.SGD_Optimizer;
    Grads          : Parameters_List;
    Updates        : Parameters_List;
    Expected       : Parameters_List;
    Momentum       : Float;
begin
    Put_Line (Routine_Name);
    Test_Common.Init (Params);

    for mom in 5 .. 9 loop
        Momentum := 0.1 * Float (mom);
--          Put_Line ("Momentum: " & Float'Image (Momentum));
        C_Init (Self => SGD, Params => Params,
                Initial_Learning_Rate => Learning_Rate, Momentum => Momentum,
                Use_Nesterov => False);
        Grads.Clear;
        SGD.Velocities.Clear;

        --  L41
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

        --  L44
        Updates.Clear;
        for index in Grads.First_Index .. Grads.Last_Index loop
            Updates.Append (Momentum * SGD.Velocities (index) -
                              Learning_Rate * Grads (index));
        end loop;

        --  L47
        Expected.Clear;
        Expected.Append (Params + Updates);
        Update_Params (SGD, Params, Grads);

--          Printing.Print_Float_Matrix
--            ("Coeff_Gradient errors", Expected (1).Coeff_Gradients -
--                 Params (1).Coeff_Gradients);
--          Printing.Print_Float_Array
--            ("Intercept_Grads errors", Expected (1).Intercept_Grads -
--                 Params (1).Intercept_Grads);

        for index in Params.First_Index .. Params.Last_Index loop
            Assert (Params (index) = Expected (index), Routine_Name &
                      "Params" & Integer'Image (index) & "," &
                      Float'Image (Params.Element (index).Intercept_Grads (1)) &
                      " does not equal expected value" & Integer'Image (index) & ","
                    & Float'Image (Expected.Element (index).Intercept_Grads (1)));
        end loop;
    end loop;

    Put_Line (Routine_Name & "passed.");

end Test_SGD;
