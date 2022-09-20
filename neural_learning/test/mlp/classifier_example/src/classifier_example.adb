--  Based on scikit-learn/sklearn/neural_network/_multilayer_perceptron.py
--  L1037 MLPClassifier example

--  with Ada.Assertions; use Ada.Assertions;
with Ada.Text_IO; use Ada.Text_IO;

with Base;
with Data_Splitter;
with Multilayer_Perceptron; use Multilayer_Perceptron;
with NL_Arrays_And_Matrices; use NL_Arrays_And_Matrices;
with NL_Types;
with Samples_Generator; use Samples_Generator;
with Test_Support;

procedure Classifier_Example is
    Routine_Name  : constant String := "Classifier_Example ";
    Num_Samples   : constant Positive := 100;
    Test_Size     : constant Positive := Num_Samples / 4;
    Train_Size    : constant Positive := Num_Samples - Test_Size;
    Weights       : NL_Types.Float_List;
    Test_Data     : constant Classification_Test_Data :=
                     Make_Classification (Num_Samples, Weights => Weights);
    X             : constant Real_Float_Matrix := Test_Data.X;
    Y             : constant Integer_Matrix := To_Integer_Matrix (Test_Data.Y);
    X_Train       : Real_Float_Matrix (1 .. Train_Size, X'Range (2));
    Y_Train       : Integer_Matrix (1 .. Train_Size, Y'Range (2));
    X_Test        : Real_Float_Matrix (1 .. Test_Size, X'Range (2));
    Y_Test        : Integer_Matrix (1 .. Test_Size, Y'Range (2));
    MLP           : MLP_Classifier := C_Init (Max_Iter => 300);
    Score         : Float;
begin
    Put_Line (Routine_Name);
    Test_Support.Print_Matrix_Dimensions (Routine_Name & "Y", Y);
    Data_Splitter.Train_Test_Split (X, Y, Train_Size, Test_Size, X_Train,
                                    Y_Train, X_Test, Y_Test);
    Test_Support.Print_Matrix_Dimensions (Routine_Name & "X_Train", X_Train);
    Test_Support.Print_Matrix_Dimensions (Routine_Name & "Y_Train", Y_Train);
    Test_Support.Print_Matrix_Dimensions (Routine_Name & "X_Test", X_Test);
    Test_Support.Print_Matrix_Dimensions (Routine_Name & "Y_Test", Y_Test);
--      Test_Support.Print_Float_Matrix (Routine_Name & "X_Train", X_Train, 1, 3);
--      Test_Support.Print_Float_Matrix (Routine_Name & "X_Test", X_Test, 1, 3);
    New_Line;
   Fit (MLP, X_Train, Y_Train);

    declare
      Predict_Prob : constant Real_Float_Matrix :=
                       Predict_ProbA (MLP, Slice (X_Test, 1, 8));
      Prediction   : constant Integer_Matrix :=
                       Predict (MLP, Slice (X_Test, 1, 8));
    begin
        Test_Support.Print_Float_Matrix (Routine_Name & "Predict_Prob",
                                         Predict_Prob);
        Test_Support.Print_Integer_Matrix (Routine_Name & "Prediction",
                                           Prediction);
    end;

    Score := Base.Score (MLP, X_Test, Y_Test);
    Put_Line ("Score: " & Float'Image (Score));

end Classifier_Example;
