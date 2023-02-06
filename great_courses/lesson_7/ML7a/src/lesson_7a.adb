
with Ada.Text_IO; use Ada.Text_IO;

with Basic_Printing; use Basic_Printing;
with ML_Arrays_And_Matrices; use ML_Arrays_And_Matrices;
with Python;
with Python_CLF;
with Python_API;

with Support_7A; use Support_7A;

procedure Lesson_7A is

   Project_Name      : constant String := "Lesson 7A ";
   Num_Samples       : constant Positive := 50;
   Train_X           : constant Real_Float_Matrix := Load_Data (Num_Samples);
   Train_Y           : constant Real_Float_Vector := Fit (Train_X);
   Test_X            : constant Real_Float_Matrix := Load_Data (Num_Samples);
   Test_Y            : constant Real_Float_Vector := Fit (Test_X);
   X_Lots            : constant Real_Float_Matrix := Load_Data (250);
   Predictions       : Real_Float_Vector (X_Lots'Range);
   Population        : constant Positive := 10000;
   Parsimony         : constant Float := 0.1;
   Classifier        : Python.Module;
   Genetic_Estimator : Python_API.PyObject;

begin
   Python.Initialize;
   Classifier := Python.Import_File ("lesson_7a");

   Python.Call (Classifier, "plot_data", Train_X, Train_Y);

   Genetic_Estimator :=
     Python.Call (Classifier, "init_SymbolicRegressor", Population, Parsimony);
   Python_CLF.Call (Classifier, "fit", Genetic_Estimator, Train_X, Train_Y);
   Predictions := Python_CLF.Call (Classifier, "predict", Genetic_Estimator,
                                   X_Lots);
   Print_Float_Vector ("Predictions", Predictions, 1, 2);
   Python.Call (Classifier, "plot_prediction", Test_X, Test_Y,
               X_Lots, Predictions);

   Python_API.Py_DecRef (Genetic_Estimator);

   Python.Finalize;

   Put_Line (Project_Name & "finished.");
   New_Line;

end Lesson_7A;
