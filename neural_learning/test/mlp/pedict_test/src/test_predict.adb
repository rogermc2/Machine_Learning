--  Based on scikit-learn/sklearn/neural_network/tests/test_mlp.py
--  test_predict_proba_binary

with Ada.Assertions; use Ada.Assertions;
with Ada.Text_IO; use Ada.Text_IO;

with Base_Neural;
with Load_Dataset;
with Multilayer_Perceptron; use Multilayer_Perceptron;
with NL_Arrays_And_Matrices; use NL_Arrays_And_Matrices;
with NL_Types;
with Test_Support; use Test_Support;

procedure Test_Predict is
    use Real_Float_Arrays;
    Routine_Name : constant String := "Test_Predict ";
    Num_Samples  : constant Integer := 50;
    Data         : constant Load_Dataset.Digits_Data_Record :=
                     Load_Dataset.Load_Digits ("../../digits.csv");
    X            : constant Real_Float_Matrix := To_Real_Float_Matrix
      (Slice (Data.Features, 1, Num_Samples));
    Y            : Integer_Matrix (1 .. Num_Samples, 1 .. 1) :=
                     (others => (others => 0));
    Layer_Sizes  : NL_Types.Integer_List;
    MLP          : MLP_Classifier;
begin
    Put_Line (Routine_Name);
    for row in Y'Range loop
        Y (row, 1) := Data.Target (row);
    end loop;

    Layer_Sizes.Append (5);
    MLP := C_Init (Hidden_Layer_Sizes => Layer_Sizes,
                   Activation => Base_Neural.Logistic_Activation);
    Fit (MLP, X, Y);

    declare
        Y_Proba     : constant Real_Float_Matrix := Predict_ProbA (MLP, X);
--          Y_Log_Proba : constant Real_Float_Matrix := Predict_Log_ProbA (MLP, X);
    begin
        Is_Probilities_Matrix (Routine_Name & "Y_Proba", Y_Proba);
        Put_Line ("Y_Proba probabilty matrix test passed.");
        Assert (Y_Proba'Length (2) = Data.Num_Classes, "Y_Proba num columns" &
                  Integer'Image (Y_Proba'Length (2)) &
                  "should equal Num_Classes" &
                  Integer'Image (Data.Num_Classes));
        Put_Line ("Num_Classes" & Integer'Image (Data.Num_Classes));
        Print_Matrix_Dimensions ("Y_Proba", Y_Proba);
--          Print_Float_Array ("Predict_Prob col sums",
--                              Classifier_Utilities.Sum_Cols (Y_Proba));
    end;

end Test_Predict;
