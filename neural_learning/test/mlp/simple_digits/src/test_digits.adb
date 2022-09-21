
with Ada.Text_IO; use Ada.Text_IO;

with Maths;

with Base;
with Classifier_Utilities;
with Load_Dataset;
with Multilayer_Perceptron;
with NL_Arrays_And_Matrices; use NL_Arrays_And_Matrices;
with NL_Types;
with Test_Support;

procedure Test_Digits is
   use Real_Float_Arrays;
   use Multilayer_Perceptron;
   Routine_Name : constant String := "Test_Digits ";
   Num_Samples  : constant Integer := 200;
   Data         : constant Load_Dataset.Digits_Data_Record :=
                          Load_Dataset.Load_Digits ("../../digits.csv");
   Features     : constant Real_Float_Matrix :=
                          To_Real_Float_Matrix (Data.Features);
   X_Train      : Real_Float_Matrix (1 .. Num_Samples, 1 .. Data.Num_Features);
   Y_Train      : Integer_Matrix (1 .. Num_Samples, 1 .. 1) :=
                          (others => (others => 0));
   X_Test       : Real_Float_Matrix (1 .. Num_Samples, 1 .. Data.Num_Features);
   Y_Test       : Integer_Matrix (1 .. Num_Samples, 1 .. 1) :=
                          (others => (others => 0));
   Index_List   : NL_Types.Integer_List;
   Indices      : Integer_Array (1 .. Num_Samples);
   Random_Index : Positive;
   Layer_Sizes  : NL_Types.Integer_List;
   aClassifier  : MLP_Classifier;
   theScore     : Float;
begin
   Put_Line (Routine_Name);
   for index in 1 .. Num_Samples loop
      Index_List.Append (index);
   end loop;

   for row in Index_List.First_Index .. Index_List.Last_Index loop
      Random_Index := Maths.Random_Integer (1, Integer (Index_List.Length));
      Indices (row) := Index_List (Random_Index);
      Index_List.Delete (Random_Index);
   end loop;

   for row in 1 .. Num_Samples loop
      Random_Index := Indices (row);
      for col in 1 .. Data.Num_Features loop
         X_Train (row, col) := Features (Random_Index, col);
         X_Test (row, col) := Features (Random_Index, col);
      end loop;
      Y_Train (row, 1) := Data.Target (Random_Index);
      Y_Test (row, 1) := Data.Target (Random_Index);
   end loop;

   Test_Support.Print_Matrix_Dimensions ("X", X_Train);
   Test_Support.Print_Matrix_Dimensions ("Y", Y_Train);

   Layer_Sizes.Append (128);
   aClassifier := C_Init (Hidden_Layer_Sizes => Layer_Sizes);
   Fit (aClassifier, X_Train, Y_Train);

    declare
      Predict_Prob : constant Real_Float_Matrix :=
                       Predict_ProbA (aClassifier, Slice (X_Test, 1, 4));
    begin
      Test_Support.Print_Float_Array
        (Routine_Name & "Predict_Prob col sums",
         Classifier_Utilities.Sum_Cols (Predict_Prob));
    end;

   theScore := Base.Score (aClassifier, X_Test, Y_Test);
   Put_Line ("Score: " & Float'Image (theScore));

end Test_Digits;
