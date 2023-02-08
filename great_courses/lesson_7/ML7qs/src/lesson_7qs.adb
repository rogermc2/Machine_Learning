
with Ada.Text_IO; use Ada.Text_IO;

--  with Maths;

--  with Basic_Printing; use Basic_Printing;
with ML_Arrays_And_Matrices; use ML_Arrays_And_Matrices;
with Python;
with Python_CLF;
with Python_API;

with Support_7Qs; use Support_7Qs;

procedure Lesson_7Qs is

   Project_Name : constant String := "Lesson 7Qs ";
   Population   : constant Positive := 10000;
   Parsimony    : constant Float := 0.1;
   Num_Samples  : constant Positive := 50;
   X            : constant Real_Float_Matrix := Load_Data (Num_Samples);
   Y            : constant Real_Float_Vector := Fit (X);
   Classifier   : Python.Module;
   Genetic_Estimator : Python_API.PyObject;
begin
   Python.Initialize;
   Classifier := Python.Import_File ("lesson_7qs");
   Genetic_Estimator :=
     Python.Call (Classifier, "init_SymbolicRegressor", Population, Parsimony);
   Python_CLF.Call (Classifier, "fit", Genetic_Estimator, X, Y);

   Python.Finalize;

   Put_Line (Project_Name & "finished.");
   New_Line;

end Lesson_7Qs;
