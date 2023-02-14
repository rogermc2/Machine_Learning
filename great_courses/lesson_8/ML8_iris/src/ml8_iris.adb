
--  with Ada.Assertions; use Ada.Assertions;
with Ada.Text_IO; use Ada.Text_IO;

with Basic_Printing; use Basic_Printing;
with CSV_Data_Loader;
with ML_Arrays_And_Matrices; use ML_Arrays_And_Matrices;
with Python;
with Python_CLF;
with Python_API;

with Support_Iris; use Support_Iris;

procedure ML8_Iris is
   use CSV_Data_Loader; use CSV_Data_Loader;
   Project_Name     : constant String := "ML8 Iris ";
   Train_Size       : constant Positive := 140;
   Test_Size        : constant Positive := 10;
   Data             : constant Base_Split_State := Get_Split_State
     ("../../../neural_learning/datasets/iris.csv", Iris_Data,
       Train_Size, Test_Size, Shuffle => True, Reload => True);
   Num_Neighbours   : constant Positive := 5;
   Classifier       : Python.Module;
   Estimator        : Python_API.PyObject;
begin
   Print_Matrix_Dimensions ("Data.Train_X", Data.Train_X);
   Print_Matrix_Dimensions ("Data.Train_Y", Data.Train_Y);

   Print_Integer_Matrix ("Data.Test_Y", Data.Test_Y);

   Python.Initialize;
   Classifier := Python.Import_File ("ml8_iris");

   Estimator :=
     Python.Call (Classifier, "init_NeighborsClassifier", Num_Neighbours);
   Python_CLF.Call (Classifier, "fit", Estimator, Data.Train_X, Data.Train_Y);

   declare
      Predictions : constant Real_Float_Vector :=
                      Python_CLF.Call (Classifier, "predict",
                                       Estimator, Data.Test_X);
      Accuracy    : constant Float :=
                      Float (Test_Score (Predictions, Data.Test_Y)) /
                      Float (Data.Test_Y'Length);
   begin
      Print_Float_Vector ("Predictions", Predictions);
      Print_Integer_Matrix ("Data.Test_Y", Data.Test_Y);
      Put_Line ("Accuracy: " & Float'Image (Accuracy));
      --     Python_CLF.Call (Classifier, "print_program", Genetic_Estimator);
      --     Python.Call (Classifier, "plot_prediction", X, Y, X_Lots, Predictions);
   end;
   Python.Finalize;

   Put_Line (Project_Name & "finished.");
   New_Line;

end ML8_Iris;
