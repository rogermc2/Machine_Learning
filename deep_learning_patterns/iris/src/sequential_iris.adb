with Ada.Assertions; use Ada.Assertions;
with Ada.Exceptions; use Ada.Exceptions;
with Ada.Text_IO;    use Ada.Text_IO;

with Basic_Printing;         use Basic_Printing;
with ML_Arrays_And_Matrices; use ML_Arrays_And_Matrices;
--  with Python;

with Iris_Support; use Iris_Support;
with Neural_Model; use Neural_Model;

--  Each neuron is a very simple function that considers a weighted sum of
--  incoming signals and then compares the value of that sum against some threshold.
procedure Sequential_Iris is
   use Real_Float_Arrays;
   Program_Name : constant String            := "Sequential_Iris ";
   Iris_Data    : constant Dataset           := Build_Dataset;
   Num_Samples  : constant Positive          := Iris_Data.Test_Length;
   Num_Features : constant Positive          := Iris_Data.Num_Features;
   Num_Classes  : constant Positive          := 1;
   Num_Epochs   : constant Positive          := 3;
   Learn_Rate   : constant Float             := 0.1;
   Input_Data   : constant Real_Float_Matrix := Iris_Data.X_Test;
   Labels       : constant Real_Float_Matrix := Iris_Data.Y_Test;
   theModel     :
     Sequential_Model
       (Num_Samples, Num_Features, Num_Classes, Loss_Mean_Square_Error);
   --     Classifier     : Python.Module;
begin
   New_Line;
   Put_Line ("Program " & Program_Name);

   Assert
     (Labels'Length = Input_Data'Length,
      Program_Name & " Labels'Length /= Input_Data'Length.");
   Print_Float_Matrix (Program_Name & "Labels", Labels);
   Add_Labels (theModel, Labels);
   Add_First_Layer (theModel, Get_Row (Input_Data, 1));
   Add_Layer (theModel, 2, Sigmoid_Activation);
   Add_Layer (theModel, 1, Identity_Activation);
   Add_Connections (theModel);

   Compile (theModel, Num_Epochs, Learn_Rate);
   declare
      Predictions : constant Real_Float_Matrix := Get_Prediction (theModel);
   begin
      Print_Float_Matrix (Program_Name & "Predicted values", Predictions);
      Print_Float_Matrix (Program_Name & "Prediction errors", Predictions - Labels);
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

end Sequential_Iris;
