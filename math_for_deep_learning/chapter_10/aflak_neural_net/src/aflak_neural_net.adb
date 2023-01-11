
with Ada.Containers;
with Ada.Text_IO; use Ada.Text_IO;

with Basic_Printing; use Basic_Printing;
with Classifier_Utilities;
with Load_Dataset;
with ML_Arrays_And_Matrices; use ML_Arrays_And_Matrices;
with Dense; use Dense;

with Network; use Network;

procedure Aflak_Neural_Net is
   use  Ada.Containers;
   use Classifier_Utilities;
   Project_Name   : constant String := "Aflak_Neural_Net ";
   Test_Size      : constant Positive := 20;
   Train_Size     : constant Positive := 1000;

   function Categorize (Labels : Integer_Matrix) return Binary_Matrix is
      Result : Binary_Matrix (Labels'Range, 0 .. 9) :=
                 (others => (others => 0));
   begin
      for row in Labels'Range loop
         Result (row, Labels (row, 1)) := 1;
      end loop;
      return Result;

   end Categorize;

   Epochs         : constant Positive := 100;
--     Epochs         : constant Positive := 20;
   Learning_Rate  : constant Float := 0.1;
   Net            : Network_Data;
   Predictions    : Real_Float_List_2D;  --  out
--     Confusion      : Integer_Matrix (0 .. 9, 0 .. 9) :=
--                        (others => (others => 0));
--     CM_Col         : Natural;
begin
   Put_Line (Project_Name);
   declare
      use Load_Dataset;
      use Real_Float_Arrays;
      Data          : constant Digits_Data_Record :=
                        Load_Digits ("../mnist_784.csv", Max_Lines => 2000);
      X_Train       : constant Real_Float_Matrix :=
                        To_Real_Float_Matrix
                          (Slice (Data.Features, 1, Train_Size));
      Y_Train       : constant Binary_Matrix :=
                        Categorize ((Slice (To_Integer_Matrix
                                    (Data.Target), 1, Train_Size)));
      X_Test        : constant Real_Float_Matrix :=
                        To_Real_Float_Matrix
                          (Slice (Data.Features, Train_Size + 1,
                           Train_Size + Test_Size));
      Y_Test        : constant Binary_Matrix :=
                        Categorize ((Slice (To_Integer_Matrix
                                    (Data.Target), Train_Size + 1,
                                    Train_Size + Test_Size)));
      Output_Data   : Real_Float_List;
   begin
      Put_Line ("Train X length: " & Count_Type'Image (X_Train'Length) & " x" &
                  Count_Type'Image (X_Train'Length (2)));
      Put_Line ("Test X length: " & Count_Type'Image (X_Test'Length));
      New_Line;
      Print_Matrix_Dimensions (Project_Name & "X_Train" , X_Train);
      Print_Matrix_Dimensions (Project_Name & "Y_Train", Y_Train);
      Print_Matrix_Dimensions (Project_Name & "X_Test", X_Test);
      Print_Matrix_Dimensions (Project_Name & "Y_Test", Y_Test);

      Add_Dense_Layer (Net.Layers, 28 * 28, 40);
      Add_Tanh_Layer (Net.Layers, 40);
      Add_Dense_Layer (Net.Layers, 40, 10);
      Add_Tanh_Layer (Net.Layers, 10);

      Train (Net, X_Train, Y_Train, Epochs, Learning_Rate);

      for sample in X_Test'Range loop
         Output_Data := To_Real_Float_List (Get_Row (X_Test, sample));
         Predict (Net, Output_Data);
         Put_Line (Integer'Image (Arg_Max (Output_Data) - 1) & ",  " &
                  Integer'Image (Arg_Max (Get_Row (Y_Test, sample))));
         Predictions.Append (Output_Data);
      end loop;

      --  Build the confusion matrix using the test set predictions
      --  Y_Test values range is the digits 0 .. 9
--        for index in Y_Test'Range loop
--           CM_Col := Arg_Max (Predictions (index)) - 1;
--           Confusion (Integer (Y_Test (index, CM_Col)), CM_Col) :=
--             Confusion (Integer (Y_Test (index, CM_Col)), CM_Col) + 1 ;
--        end loop;
--
--        Print_Integer_Matrix ("Confusion matrix", Confusion);
--        Put_Line ("Confusion diagonal sum:" &
--                    Integer'Image (Sum_Diagonal (Confusion)));
--        Put_Line ("Confusion sum:" & Integer'Image (Sum (Confusion)));
--        Put_Line ("Accuracy: " & Float'Image (Float (Sum_Diagonal (Confusion)) /
--                    Float (Sum (Confusion))));
   end;

   Put_Line (Project_Name & "done.");
   New_Line;

end Aflak_Neural_Net;
