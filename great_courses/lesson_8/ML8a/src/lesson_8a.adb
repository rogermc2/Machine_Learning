
with System;

with Ada.Assertions; use Ada.Assertions;
with Ada.Text_IO; use Ada.Text_IO;

--  with Basic_Printing; use Basic_Printing;
with CSV_Data_Loader;
with ML_Arrays_And_Matrices; use ML_Arrays_And_Matrices;
with Python;
with Python_CLF;
with Python_API;

with Support_8A; use Support_8A;

procedure Lesson_8A is
   use System;
   use CSV_Data_Loader;
   Project_Name     : constant String := "Lesson 8A ";
   Train_Data       : constant Data_Record :=
     Get_Data ("../../data/malware-train.csv");
   Test_Data        : constant Data_Record :=
     Get_Data ("../../data/malware-test.csv");
   Train_Size       : constant Positive := Train_Data.Features'Length;
   Test_Size        : constant Positive := Test_Data.Features'Length;
   Tests            : constant Integer_Array (1 .. 4) := (1, 5, 7,9);
   Num_Neighbours   : Positive;
   Classifier       : Python.Module;
   Estimator        : Python_API.PyObject;
begin
--     for index in X'Range loop
--        X (index, 1) := Train_Data.Features (index);
--        Y (index) := Train_Data.Labels (index);
--     end loop;

   Python.Initialize;
   Classifier := Python.Import_File ("lesson_8a");
   Assert (Classifier /= Null_Address, Project_Name &
             "Import_File returned null.");

   Num_Neighbours := Tests (1);
   Estimator :=
     Python.Call (Classifier, "init_NeighborsClassifier", Num_Neighbours);
   Assert (Estimator /= Null_Address, Project_Name &
             "init_NeighborsClassifier returned null.");
   Python_CLF.Call (Classifier, "fit", Estimator, Train_Data.Features, Train_Data.Labels);

   declare
      Predictions : Real_Float_Vector :=
        Python_CLF.Call (Classifier, "predict",  Estimator, Test_Data.Features);
      Accuracy    : Float :=
        Float (Test_Score (Predictions, Test_Data.Labels)) / Float (Test_Size);
   begin
      Put_Line ("Accuracy: " & Float'Image (Accuracy));
      --     Python_CLF.Call (Classifier, "print_program", Genetic_Estimator);
      --     Python.Call (Classifier, "plot_prediction", X, Y, X_Lots, Predictions);
   end;
   Python.Finalize;

   Put_Line (Project_Name & "finished.");
   New_Line;

end Lesson_8A;
