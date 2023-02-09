
with Ada.Text_IO; use Ada.Text_IO;

--  with Basic_Printing; use Basic_Printing;
with ML_Arrays_And_Matrices; use ML_Arrays_And_Matrices;
with Python;
--  with Python_CLF;
with Python_API;

with Support_8A; use Support_8A;

procedure Lesson_8A is

   Project_Name : constant String := "Lesson 8A ";
   --     Num_Samples  : constant Positive := 50;
   Data         : Data_Record;
--     X            : constant Real_Float_Matrix := Load_Data (Num_Samples);
--     Y            : constant Real_Float_Vector := Fit (X);
--     Predictions  : Real_Float_Vector (X_Lots'Range);
   Classifier   : Python.Module;
   Estimator    : Python_API.PyObject;
begin
   Python.Initialize;
   Classifier := Python.Import_File ("lesson_8a");
   Estimator :=
     Python.Call (Classifier, "init_NeighborsClassifier");
--     Python_CLF.Call (Classifier, "fit", Genetic_Estimator, X, Y);
--
--     Predictions := Python_CLF.Call (Classifier, "predict", Genetic_Estimator,
--                                     X_Lots);
--     Python_CLF.Call (Classifier, "print_program", Genetic_Estimator);
--     Python.Call (Classifier, "plot_prediction", X, Y, X_Lots, Predictions);

   Python.Finalize;

   Put_Line (Project_Name & "finished.");
   New_Line;

end Lesson_8A;
