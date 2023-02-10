
with System;

with Ada.Assertions; use Ada.Assertions;
with Ada.Text_IO; use Ada.Text_IO;

--  with Basic_Printing; use Basic_Printing;
with ML_Arrays_And_Matrices; use ML_Arrays_And_Matrices;
with Python;
with Python_CLF;
with Python_API;

with Support_8A; use Support_8A;

procedure Lesson_8A is
   use System;
   Project_Name     : constant String := "Lesson 8A ";
   Num_Samples      : constant Positive := 4000;
   Train_Data       : constant Data_Record :=
                        Load_Data ("../../data/malware-train.csv");
   Test_Data        : constant Data_Record :=
                        Load_Data ("../../data/malware-test.csv");
   X                : Real_Float_Vector (1 .. Num_Samples);
   Y                : Integer_Array (1 .. Num_Samples);
   Classifier       : Python.Module;
   Estimator        : Python_API.PyObject;
begin
   for index in X'Range loop
      X (index) := Train_Data.Features (index);
      Y (index) := Train_Data.Labels (index);
   end loop;

   Python.Initialize;
   Classifier := Python.Import_File ("lesson_8a");
   Assert (Classifier /= Null_Address, Project_Name &
             "Import_File returned null.");

   Estimator :=
     Python.Call (Classifier, "init_NeighborsClassifier", 1);
   Assert (Estimator /= Null_Address, Project_Name &
             "init_NeighborsClassifier returned null.");
   Python_CLF.Call (Classifier, "fit", Estimator, X, Y);

   declare
      Predictions : Real_Float_Vector :=
                      Python_CLF.Call (Classifier, "predict",
                                       Estimator, Train_Data.Features);
   begin
      null;
       --     Python_CLF.Call (Classifier, "print_program", Genetic_Estimator);
      --     Python.Call (Classifier, "plot_prediction", X, Y, X_Lots, Predictions);
   end;
   Python.Finalize;

   Put_Line (Project_Name & "finished.");
   New_Line;

end Lesson_8A;
