--  Based on scikit-learn/sklearn/neural_network/_multilayer_perceptron.py
--  L1037 MLPClassifier example

--  with Ada.Assertions; use Ada.Assertions;
with Ada.Text_IO; use Ada.Text_IO;

with Data_Splitter;
with Multilayer_Perceptron; use Multilayer_Perceptron;
with NL_Arrays_And_Matrices; use NL_Arrays_And_Matrices;
--  with NL_Types;
with Samples_Generator; use Samples_Generator;
with Test_Support;

procedure Classifier_Example is
    Routine_Name  : constant String := "Classifier_Example ";
    Num_Samples   : constant Positive := 100;
    Test_Size     : constant Positive := Num_Samples / 4;
    Train_Size    : constant Positive := Num_Samples - Test_Size;
    Test_Data     : constant Classification_Test_Data :=
                      Make_Multilabel_Classification (Num_Samples);
    X             : constant Real_Float_Matrix := Test_Data.X;
    Y             : constant Integer_Matrix := To_Integer_Matrix (Test_Data.Y);
    X_Train       : Real_Float_Matrix (1 .. Train_Size, X'Range (2));
    Y_Train       : Integer_Matrix (1 .. Train_Size, Y'Range (2));
    X_Test        : Real_Float_Matrix (1 .. Test_Size, X'Range (2));
    Y_Test        : Integer_Matrix (1 .. Test_Size, Y'Range (2));
    MLP           : MLP_Classifier := C_Init (Max_Iter => 300);

begin
    Put_Line (Routine_Name);
    Data_Splitter.Train_Test_Split (X, Y, Train_Size, Test_Size, X_Train,
                                    Y_Train, X_Test, Y_Test);
    Fit (MLP, X_Train, Y_Train);
    pred

    Test_Support.Print_Matrix_Dimensions ("X", X);
    Test_Support.Print_Matrix_Dimensions ("Y", Y);

end Classifier_Example;
