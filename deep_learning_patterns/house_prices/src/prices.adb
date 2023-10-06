with Ada.Assertions; use Ada.Assertions;
with Ada.Exceptions; use Ada.Exceptions;
with Ada.Text_IO;    use Ada.Text_IO;

with Basic_Printing;         use Basic_Printing;
with ML_Arrays_And_Matrices; use ML_Arrays_And_Matrices;
--  with Python;

with Prices_Support; use Prices_Support;
with Neural_Model; use Neural_Model;

--  Each neuron is a very simple function that considers a weighted sum of
--  incoming signals and then compares the value of that sum against some threshold.
procedure Prices is
   use Real_Float_Arrays;
   Program_Name : constant String            := "House Prices ";
   Train_Length : constant Positive := 70;
   Test_Length  : constant Positive := 3;
   Data         : constant Dataset           := Build_Dataset (Train_Length,
                                                               Test_Length, 6);
   Num_Samples  : constant Positive          := Test_Length;
   Num_Features : constant Positive          := Data.Num_Features;
   Num_Epochs   : constant Positive          := 1;
   Learn_Rate   : constant Float             := 1.0;
   Input_Data   : constant Real_Float_Matrix := Data.X_Test;
   Labels       : constant Real_Float_Matrix := Data.Y_Test;
   Num_Classes  : constant Positive          := Labels'Length (2);
   theModel     : Sequential_Model (Num_Samples, Num_Features, Num_Classes,
                                    Loss_Mean_Square_Error);
   --     Classifier     : Python.Module;

begin
   New_Line;
   Put_Line ("Program " & Program_Name);

   Assert
     (Labels'Length = Input_Data'Length,
      Program_Name & " Labels'Length /= Input_Data'Length.");
   Print_Float_Matrix (Program_Name & "Input_Data", Input_Data);
   Print_Float_Matrix (Program_Name & "Labels", Labels);
   Put_Line ("Program " & "Num_Features" & Integer'Image (Num_Features));

   Add_Data (theModel, Input_Data, Labels);
   Add_First_Layer (theModel);
   Add_Layer (theModel, Num_Features - 2, Sigmoid_Activation);
   Add_Layer (theModel, 1, Identity_Activation);
   Add_Connections (theModel);

   Compile (theModel, Num_Epochs, Learn_Rate);
   declare
      Predictions : constant Real_Float_Matrix := Get_Prediction (theModel);
   begin
--        Print_Float_Matrix (Program_Name & "Actual values", Labels, 1, 5);
--        Print_Float_Matrix (Program_Name & "Predicted values", Predictions, 1, 5);
      Print_Float_Matrix
        (Program_Name & "Prediction errors", Predictions - Labels, 1, 5);
   end;

   --     Python.Initialize;
   --     Classifier := Python.Import_File ("sequential");
   --
   --     Python.Call (Classifier, "plot", X);
   --
   --     Python.Finalize;

   Put_Line ("----------------------------------------------");

   Put_Line (Program_Name & "finished.");
   New_Line;

exception
   when Error : Constraint_Error =>
      Put_Line (Program_Name & "Constraint_Error");
      Put_Line (Exception_Information (Error));
   when Error : others           =>
      Put_Line (Program_Name & "exception");
      Put_Line (Exception_Information (Error));

end Prices;
