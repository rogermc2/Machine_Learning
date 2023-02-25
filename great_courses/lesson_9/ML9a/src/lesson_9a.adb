
with Ada.Text_IO; use Ada.Text_IO;

--  with ML_Types; use ML_Types;

--  with Basic_Printing; use Basic_Printing;
with CSV_Data_Loader;
--  with ML_Arrays_And_Matrices; use ML_Arrays_And_Matrices;
with Python;
with Python_API;

procedure Lesson_9A is
   use CSV_Data_Loader;
   Routine_Name  : constant String := "Lesson 9A ";
   Train_Size    : constant Positive := 768 / 2;
   Test_Size     : constant Positive := Train_Size;
   Data          : constant Base_Split_State :=
                     Get_Split_State ("../../data/diabetes.csv", Diabetes_Data,
                                      Train_Size, Test_Size, Shuffle => True,
                                      Reload => True);
   Classifier    : Python.Module;
   Estimator     : Python_API.PyObject;
begin
   Python.Initialize;

   Classifier := Python.Import_File ("lesson_9a");
   for degree in 0 .. 7 loop
      Estimator :=
        Python.Call (Classifier, "init_svm", degree);

      Python.Call (Classifier, "fit", Data.Train_X, Data.Train_Y);
      Python.Call (Classifier, "predict", Data.Train_X);
      Python_API.Py_DecRef (Estimator);
   end loop;

   Python.Close_Module (Classifier);
   Python.Finalize;

   Put_Line (Routine_Name & "finished.");
   Put_Line ("----------------------------------------------");
   New_Line;

end Lesson_9A;
