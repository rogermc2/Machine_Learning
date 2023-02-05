--  From https://gplearn.readthedocs.io/en/stable/examples.html

with Ada.Text_IO; use Ada.Text_IO;

with Python;
with Python_API;
with Python_CLF;

--  with Support; use Support;

procedure GP_Learn is

   Project_Name : constant String := "GP Learn ";
--     Num_Samples  : constant Positive := 50;
--     Train_X      : constant Real_Float_Matrix := Load_Data (Num_Samples);
--     Train_Y      : constant Real_Float_Vector := Fit (Train_X);
--     Test_X       : constant Real_Float_Vector := Load_Data (Num_Samples);
--     Test_Y       : constant Real_Float_Vector := Fit (Test_X);
--     Population_Size   : constant Integer := 10000;
--     Parsimony_Coeff   : constant Float := 0.1;
   Classifier        : Python.Module;
   Genetic_Estimator : Python_API.PyObject;

begin
   Python.Initialize;
   Classifier := Python.Import_File ("gp_learn");

--     Python.Call (Classifier, "plot_data");
--     Python.Call (Classifier, "plot_data", Train_X, Train_Y);

   Genetic_Estimator :=
     Python.Call (Classifier, "init_SymbolicRegressor");
   Python_CLF.Call (Classifier, "fit", Genetic_Estimator);
--       Python.Call (Classifier, "init_SymbolicRegressor", Population_Size, Parsimony_Coeff);
--     Python_CLF.Call (Classifier, "fit", Genetic_Estimator, Train_X, Train_Y);
--
--     Python_API.Py_DecRef (Genetic_Estimator);

   Python.Finalize;

   Put_Line (Project_Name & "finished.");
   New_Line;

end GP_Learn;
