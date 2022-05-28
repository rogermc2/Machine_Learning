
with Ada.Text_IO; use Ada.Text_IO;

with Load_Dataset;
with Multilayer_Perceptron;
with NL_Arrays_And_Matrices; use NL_Arrays_And_Matrices;
with NL_Types;
with Printing;
with Stochastic_Optimizers;

procedure Test_Alpha is
    use Real_Float_Arrays;
    use Multilayer_Perceptron;
    use Stochastic_Optimizers;

    Routine_Name  : constant String := "Test_Alpha ";
    Data          : constant Load_Dataset.Data_Record :=
                      Load_Dataset.Load_Digits;
    Features      : constant Real_Float_Matrix :=
                      To_Real_Float_Matrix (Data.Features);
    X             : Real_Float_Matrix (1 .. 100, 1 .. Data.Num_Features);
    Y             : Integer_Matrix (1 .. 100, 1 .. 2) :=
                      (others => (others => 0));
    Layer_Sizes   : NL_Types.Integer_List;
    Absolute_Sum  : Float := 0.0;
    Sum           : Float := 0.0;
    Coeffs_Sum    : NL_Types.Float_List;
    Alpha_Vectors : NL_Types.Float_List_2D;
    aClassifier   : MLP_Classifier;

    --  -------------------------------------------------------------------------

begin
    Put_Line (Routine_Name);
    for row in 1 .. 100 loop
        for col in 1 .. Data.Num_Features loop
            X (row, col) := Features (row, col);
        end loop;
        Y (row, 1) := Data.Target (row);
    end loop;

    for row in X'Range loop
        for col in X'Range (2) loop
            Absolute_Sum := Absolute_Sum + abs (X (row, col));
        end loop;
    end loop;

    Layer_Sizes.Append (10);
    for alpha_value in 1 .. 3 loop
        aClassifier := C_Init
          (Alpha => Float (alpha_value), Random_State => 1,
           Hidden_Layer_Sizes => Layer_Sizes);
        --          Init_Optimizer (aClassifier);
        Fit (aClassifier, X, Y);
        Coeffs_Sum.Clear;
        Sum := 0.0;
        for index in 1 .. 2 loop
            declare
                Params       : constant Parameters_Record :=
                                 aClassifier.Attributes.Params.Element (index);
                Coeff_Matrix : constant Real_Float_Matrix :=
                                 Params.Coeff_Gradients;
            begin
                for row in Coeff_Matrix'Range loop
                    for col in Coeff_Matrix'Range (2) loop
                        Sum := Sum + abs (Coeff_Matrix (row, col));
                    end loop;
                end loop;
            end;
        end loop;

        Coeffs_Sum.Append (Sum);
        Alpha_Vectors.Append (Coeffs_Sum);
    end loop;

    Printing.Print_Float_List ("Alpha_Vectors 1", Alpha_Vectors.Element (1));

end Test_Alpha;
