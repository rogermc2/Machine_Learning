
with System;

with Ada.Assertions; use Ada.Assertions;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;

--  with Basic_Printing; use Basic_Printing;
with Classifier_Utilities;
with CSV_Data_Loader;
with ML_Arrays_And_Matrices; use ML_Arrays_And_Matrices;
with ML_Types;
with Python;
with Python_CLF;
with Python_API;

with Support_Iris; use Support_Iris;

procedure ML8_Iris is
   use System;
   use CSV_Data_Loader;
   use ML_Types;
   Project_Name     : constant String := "ML8 Iris ";
   Train_Size       : constant Positive := 4000;
   Test_Size        : constant Positive := 2000;
   Iris_Data        : constant Base_Split_State :=
                        Get_Split_State ("iris.csv", Iris_Data, Test_Size,
                                         Y_Categorized => True,
                                         Shuffle       => False,
                                         Reload        => False);
   --     Iris_Data        : constant Multi_Output_Data_Record :=
   --                          Classifier_Utilities.Load_Data
   --                            ("../../../neural_learning/datasets/iris.csv");
   --     Iris_Features    : constant Value_Data_Lists_2D :=
   --                          Iris_Data.Feature_Values;
   --     Iris_Labels      : constant Value_Data_Lists_2D :=  Iris_Data.Label_Values;
   Class_Names      : Class_Names_List;
   X                : Real_Float_Matrix (1 .. Num_Samples, 1 .. 1);
   Y                : Integer_Array (1 .. Num_Samples);
   Classes          : Integer_Array (1 .. 3);
   Num_Neighbours   : Positive;
   Classifier       : Python.Module;
   Estimator        : Python_API.PyObject;
begin
   Class_Names.Append (To_Unbounded_String ("Setosa"));
   Class_Names.Append (To_Unbounded_String ("Versicolour"));
   Class_Names.Append (To_Unbounded_String ("Virginica"));
   Classes := Categorize (Class_Names);

   for index in X'Range loop
      X (index, 1) := Iris_Data.Feature_Values (index);
      Y (index) := Iris_Data.Label_Values (index);
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
