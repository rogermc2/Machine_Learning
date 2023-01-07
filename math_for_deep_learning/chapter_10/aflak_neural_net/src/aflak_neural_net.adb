
--  with Ada.Containers;
with Ada.Text_IO; use Ada.Text_IO;

with Basic_Printing; use Basic_Printing;
with Classifier_Utilities;
with ML_Arrays_And_Matrices; use ML_Arrays_And_Matrices;
with Dense; use Dense;
with Support_4;

with Network; use Network;

procedure Aflak_Neural_Net is
--     use  Ada.Containers;
   use Support_4;
   use Classifier_Utilities;
   Project_Name   : constant String := "Aflak_Neural_Net ";
   Dataset_Name   : constant String := "mnist_784";
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
   Learning_Rate  : constant Float := 0.1;
   Net            : Network_Data;
   Predictions    : Real_Float_List_2D;  --  out
   Confusion      : Integer_Matrix (0 .. 9, 0 .. 9) :=
     (others => (others => 0));
   CM_Col         : Natural;
begin
   declare
      Data          : constant Base_State :=
        Get_State (Dataset_Name, Train_Size, Test_Size);
      X_Train       : constant Real_Float_Matrix := Data.Train_X / 255.0;
      Y_Train       : constant Binary_Matrix := Categorize (Data.Train_Y);
      X_Test        : constant Real_Float_Matrix := Data.Test_X / 255.0;
      Y_Test        : constant Binary_Matrix := Categorize (Data.Test_Y);
   begin
      Put_Line ("Train X length: " & Count_Type'Image (X_Train'Length) & " x" &
                  Count_Type'Image (X_Train'Length (2)));
      Put_Line ("Test X length: " & Count_Type'Image (X_Test'Length));
      New_Line;
      Print_Matrix_Dimensions (Project_Name & "X_Train" , X_Train);
      Print_Matrix_Dimensions (Project_Name & "Y_Train", Y_Train);
      Print_Matrix_Dimensions (Project_Name & "X_Test", X_Test);
      Put_Line (Project_Name & "Y_Test length:" &  Integer'Image (Y_Test'Length));

      Add_Fully_Connected_Layer (Net.Layers, 28 * 28, 40);
      Add_Activation_Layer (Net.Layers, 40);
      Add_Fully_Connected_Layer (Net.Layers, 40, 10);
      Add_Activation_Layer (Net.Layers, 10);

      Train (Net, X_Train, Y_Train, Epochs, Learning_Rate);

      --  Build the confusion matrix using the test set predictions
      Predictions := Predict (Net, X_Test);  --  out
      Put_Line ("Predictions size:" & Integer'Image (Integer (Predictions.Length))
                & " x" & Integer'Image (Integer (Predictions (1).Length)));
      Print_Binary_Matrix ("Y_Test", Y_Test, 1, 1);
      Print_Real_Float_List_2D ("Predictions", Predictions, 1, 1);

      --  Y_Test values range is the digits 0 .. 9
      for index in Y_Test'Range loop
         CM_Col := Arg_Max (Predictions (index)) - 1;
         Confusion (Integer (Y_Test (index, CM_Col)), CM_Col) :=
           Confusion (Integer (Y_Test (index, CM_Col)), CM_Col) + 1 ;
      end loop;

      Print_Integer_Matrix ("Confusion matrix", Confusion);
      Put_Line ("Confusion diagonal sum:" &
                  Integer'Image (Sum_Diagonal (Confusion)));
      Put_Line ("Confusion sum:" & Integer'Image (Sum (Confusion)));
      Put_Line ("Accuracy: " & Float'Image (Float (Sum_Diagonal (Confusion)) /
                  Float (Sum (Confusion))));
   end;

   Put_Line (Project_Name & "done.");
   New_Line;

end Aflak_Neural_Net;
