
with Ada.Text_IO; use Ada.Text_IO;

with Maths;

with Base;
with Multilayer_Perceptron;
with NL_Arrays_And_Matrices; use NL_Arrays_And_Matrices;
with NL_Types;
--  with Stochastic_Optimizers;
--  with Test_Support;
with Utilities;

procedure Data_Flow_2D is
    use Real_Float_Arrays;
    use Multilayer_Perceptron;
    --     use Stochastic_Optimizers;

    Routine_Name       : constant String := "Data_Flow_2D ";
    Num_Samples        : constant Integer := 75;
    X                  : Real_Float_Matrix (1 .. 100, 1 .. 2);
    Y                  : Integer_Array (1 .. 100) := (1 .. 50 => 0,
                                                      51 .. 100 => 1);
    X_Train            : Real_Float_Matrix (1 .. Num_Samples, 1 .. 2);
    Y_Train            : Integer_Matrix (1 .. Num_Samples, 1 .. 1);
    X_Test             : Real_Float_Matrix (1 .. Num_Samples, 1 .. 2);
    Y_Test             : Integer_Matrix (1 .. Num_Samples, 1 .. 1);
    Layer_Sizes        : NL_Types.Integer_List;
    aClassifier        : MLP_Classifier;
    theScore           : Float;
begin
    Put_Line (Routine_Name);
    for row in 1 .. 50 loop
        X (row, 1) := Maths.Random_Float - 0.3;
        X (row, 2) := Maths.Random_Float + 0.3;
    end loop;

    for row in 51 .. 100 loop
        X (row, 1) := Maths.Random_Float + 0.3;
        X (row, 2) := Maths.Random_Float - 0.3;
    end loop;

    X := Utilities.Permute (X);
    Utilities.Permute (Y);

    for row in 1 .. 75 loop
        for col in 1 .. 2 loop
            X_Train (row, col) := X (row, col);
        end loop;
        Y_Train (row, 1) := Y (row);
    end loop;

    for row in 1 .. 25 loop
        for col in 1 .. 2 loop
            X_Test (row, col) := X (75 + row, col);
        end loop;
        Y_Test (row, 1) := Y (75 + row);
    end loop;

    Layer_Sizes.Append (5);
    aClassifier := C_Init (Hidden_Layer_Sizes => Layer_Sizes);
    Fit (aClassifier, X_Train, Y_Train);

    theScore := Base.Score (aClassifier, X_Test, Y_Test);
    Put_Line ("Score: " & Float'Image (theScore));

end Data_Flow_2D;
