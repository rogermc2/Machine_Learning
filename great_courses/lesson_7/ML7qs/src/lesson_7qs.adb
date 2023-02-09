
--  That is, it solves a regression problem by finding a symbolic expression
--  that is a small piece of a program.
--  The program it finds will use 'add' for add, 'mul' for multiply and
--  'abs' for absolute value.
--  A program can use as many of these operators as necessary.

--  training data and being parsimonious; that is, using a small expression.
--  The parsimony_coefficient tells the genetic programming search how much
--  weight to put on parsimony compared to accuracy.
--  The parsimony coefficient can be any non-negative value, where
--  smaller numbers tend to result in much bigger programs.

--  Lesson_7Qs trains a symbolic regression using genetic programming and
--  tries to fit the model to a set of points.

with Ada.Text_IO; use Ada.Text_IO;

--  with Basic_Printing; use Basic_Printing;
with ML_Arrays_And_Matrices; use ML_Arrays_And_Matrices;
with Python;
with Python_CLF;
with Python_API;

with Support_7Qs; use Support_7Qs;

procedure Lesson_7Qs is

   Project_Name : constant String := "Lesson 7Qs ";
   Population   : constant Positive := 10000;
   Parsimony    : constant Float := 0.01;
   Num_Samples  : constant Positive := 50;
   X            : constant Real_Float_Matrix := Load_Data (Num_Samples);
   Y            : constant Real_Float_Vector := Fit (X);
   X_Lots       : constant Real_Float_Matrix :=  Load_Sorted_Data (250);
   Predictions  : Real_Float_Vector (X_Lots'Range);
   Classifier   : Python.Module;
   Genetic_Estimator : Python_API.PyObject;
begin
   Python.Initialize;
   Classifier := Python.Import_File ("lesson_7qs");
   Genetic_Estimator :=
     Python.Call (Classifier, "init_SymbolicRegressor", Population, Parsimony);
   Python_CLF.Call (Classifier, "fit", Genetic_Estimator, X, Y);

   Predictions := Python_CLF.Call (Classifier, "predict", Genetic_Estimator,
                                   X_Lots);
   Python_CLF.Call (Classifier, "print_program", Genetic_Estimator);
   Python.Call (Classifier, "plot_prediction", X, Y, X_Lots, Predictions);

   Python.Finalize;

   Put_Line (Project_Name & "finished.");
   New_Line;

end Lesson_7Qs;
