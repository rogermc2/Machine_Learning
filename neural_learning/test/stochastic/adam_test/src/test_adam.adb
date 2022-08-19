
--  Based on scikit-learn/sklearn/neural_network/tests/
--  test_stochastic_optimizers.py

with Ada.Assertions; use  Ada.Assertions;
with Ada.Text_IO; use Ada.Text_IO;

with Maths;

with NL_Arrays_And_Matrices; use NL_Arrays_And_Matrices;
with Test_Support;
with Stochastic_Optimizers;
with Test_Common;

--  Based on  test_adam_optimizer()
procedure Test_Adam is
    use Real_Float_Arrays;
    use Maths.Float_Math_Functions;
    use Stochastic_Optimizers;
    use Test_Common;
    Routine_Name   : constant String := "Test_Adam ";
    Epsilon        : constant Float := 10.0 ** (-8);
    LR             : constant Float := 0.001;
    Beta_1         : constant Real_Float_Vector (1 .. 2) :=  (0.9, 0.95);
    Beta_2         : constant Real_Float_Vector (1 .. 6) :=
                       (0.995, 0.996, 0.997, 0.998, 0.999, 1.0);
    T              : constant Natural := 10;
    Params         : Parameters_List;
    Adam           : Stochastic_Optimizers.Adam_Optimizer;
    First_Moments  : Moments_List;
    Second_Moments : Moments_List;
    Grads          : Parameters_List;
    Learning_Rate  : Float;
    Updates        : Parameters_List;
    Expected       : Parameters_List;
begin
    Put_Line (Routine_Name);
    Test_Common.Init (Params);

    --  L92
    for b1 in Beta_1'First .. Beta_1'Last loop
        for b2 in Beta_2'First .. Beta_2'Last loop
            C_Init (Self => Adam, Params => Params,
                    Initial_Learning_Rate => LR,
                    Beta_1 => Beta_1 (b1), Beta_2 => Beta_2 (b2));
            --  L95 Initialize first and second moments and ggradients
            First_Moments.Clear;
            Second_Moments.Clear;
            Grads.Clear;
            for shape in Shapes.First_Index .. Shapes.Last_Index loop
                declare
                    Bounds    : constant Integer_Array := Shapes.Element (shape);
                    First_Mom : Parameters_Record (Bounds (1), Bounds (2));
                    Sec_Mom   : Parameters_Record (Bounds (1), Bounds (2));
                    GR        : Parameters_Record (Bounds (1), Bounds (2));
                begin
                    for row in First_Mom.Coeff_Gradients'Range loop
                        for col in First_Mom.Coeff_Gradients'Range (2) loop
                            First_Mom.Coeff_Gradients (row, col) :=
                              abs (Maths.Random_Float);
                            Sec_Mom.Coeff_Gradients (row, col) :=
                              abs (Maths.Random_Float);
                            GR.Coeff_Gradients (row, col) := abs (Maths.Random_Float);
                        end loop;
                    end loop;

                    for row in First_Mom.Intercept_Grads'Range loop
                        First_Mom.Intercept_Grads (row) := abs (Maths.Random_Float);
                        Sec_Mom.Intercept_Grads (row) := abs (Maths.Random_Float);
                        GR.Intercept_Grads (row) := abs (Maths.Random_Float);
                    end loop;

                    First_Moments.Append (First_Mom);
                    Second_Moments.Append (Sec_Mom);
                    --  L101
                    Grads.Append (GR);
                end;
            end loop;

            --  L98  Set inital values of Adam first and second moments
            Adam.First_Moments := First_Moments;
            Adam.Second_Moments := Second_Moments;
            Adam.Time_Step := T - 1;

            --  L103 Update local first and second moments
            for index in First_Moments.First_Index ..
              First_Moments.Last_Index loop
                First_Moments (index).Coeff_Gradients :=
                  Beta_1 (b1) * First_Moments (index).Coeff_Gradients +
                  (1.0 - Beta_1 (b1)) * Grads (index).Coeff_Gradients;
                First_Moments (index).Intercept_Grads :=
                  Beta_1 (b1) * First_Moments (index).Intercept_Grads +
                  (1.0 - Beta_1 (b1)) * Grads (index).Intercept_Grads;
                declare
                    Grad_Sq : constant Parameters_Record :=
                                Square (Grads (index));
                begin
                    Second_Moments (index).Coeff_Gradients :=
                      Beta_2 (b2) * Second_Moments (index).Coeff_Gradients +
                      (1.0 - Beta_2 (b2)) * Grad_Sq.Coeff_Gradients;
                    Second_Moments (index).Intercept_Grads :=
                      Beta_2 (b2) * Second_Moments (index).Intercept_Grads +
                      (1.0 - Beta_2 (b2)) * Grad_Sq.Intercept_Grads;
                end;
            end loop;

            --  L105
            Learning_Rate := LR * Sqrt (1.0 - Beta_2 (b2) ** T) /
              (1.0 - Beta_1 (b1) ** T);
            --  L106 Set Updates
            Updates.Clear;
            for index in First_Moments.First_Index ..
              First_Moments.Last_Index loop
                declare
                    FM      : Parameters_Record := First_Moments (index);
                    SM      : constant Parameters_Record :=
                                Second_Moments (index);
                    SM_Sqrt : constant Parameters_Record := Sqrt (SM, Epsilon);
                begin
                    FM := (- Learning_Rate) * FM / SM_Sqrt;
                    Updates.Append (FM);
                end;
            end loop;
            Expected := Params + Updates;

            Update_Params (Adam, Params, Grads);

            for index in Params.First_Index .. Params.Last_Index loop
                Assert (Test_Support.Almost_Equal (Params (index),
                        Expected (index), -8), Routine_Name & "Params" &
                          Integer'Image (index) &
                          " does not equal expected value");
            end loop;
        end loop;
        New_Line;
    end loop;

end Test_Adam;
