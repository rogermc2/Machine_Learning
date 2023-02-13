
with System;

with Ada.Assertions; use Ada.Assertions;
with Ada.Text_IO; use Ada.Text_IO;

--  with Basic_Printing; use Basic_Printing;
with ML_Arrays_And_Matrices; use ML_Arrays_And_Matrices;
with Python;
with Python_CLF;
with Python_API;

with Support_Iris; use Support_Iris;

procedure ML8_Iris is
   use System;
   Project_Name     : constant String := "ML8 Iris ";
   Num_Samples      : constant Positive := 4000;
   Train_Data       : constant Data_Record := Load_Data
     ("../../../neural_learning/datasets/iris.csv");
   Tests            : constant Integer_Array (1 .. 4) := (1, 5, 7,9);
   X                : Real_Float_Matrix (1 .. Num_Samples, 1 .. 1);
   Y                : Integer_Array (1 .. Num_Samples);
   Num_Neighbours   : Positive;
   Classifier       : Python.Module;
   Estimator        : Python_API.PyObject;
begin
   for index in X'Range loop
      X (index, 1) := Train_Data.Features (index);
      Y (index) := Train_Data.Labels (index);
   end loop;

   Python.Initialize;
   Classifier := Python.Import_File ("ml8_iris");
   Assert (Classifier /= Null_Address, Project_Name &
             "Import_File returned null.");

   Num_Neighbours := Tests (1);
   Estimator :=
     Python.Call (Classifier, "init_NeighborsClassifier", Num_Neighbours);
   Assert (Estimator /= Null_Address, Project_Name &
             "init_NeighborsClassifier returned null.");
   Python_CLF.Call (Classifier, "fit", Estimator, X, Y);

   declare
      Predictions : Real_Float_Vector :=
                      Python_CLF.Call (Classifier, "predict",  Estimator, X);
      Accuracy    : Float :=
                      Float (Test_Score (Predictions, Y)) / Float (Num_Samples);
   begin
      Put_Line ("Accuracy: " & Float'Image (Accuracy));
       --     Python_CLF.Call (Classifier, "print_program", Genetic_Estimator);
      --     Python.Call (Classifier, "plot_prediction", X, Y, X_Lots, Predictions);
   end;
   Python.Finalize;

   Put_Line (Project_Name & "finished.");
   New_Line;

end ML8_Iris;
