
with Ada.Text_IO; use Ada.Text_IO;

--  with Basic_Printing; use Basic_Printing;
with CSV_Data_Loader;
with ML_Arrays_And_Matrices; use ML_Arrays_And_Matrices;
with Python;
with Python_API;
with Python_CLF;
with Support_9A; use Support_9A;

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
   --     for degree in 0 .. 7 loop
   for degree in 0 .. 5 loop
      Estimator := Python.Call (Classifier, "init_svc", degree);

      Put_Line (Routine_Name & "fitting degree: " & Integer'Image (degree));
      Python_CLF.Call (Classifier, "fit", Estimator, Data.Train_X,
                       Data.Train_Y);
      Put_Line (Routine_Name & "fitted");
      declare
         Predictions : constant Real_Float_Vector :=
                         Python_CLF.Call (Classifier, "predict", Estimator, Data.Train_X);
         Accuracy    : constant Float :=
                         Float (Test_Score (Predictions, Data.Test_Y)) /
                         Float (Data.Test_Y'Length);
      begin
--           Print_Float_Vector ("Predictions", Predictions);
--           Print_Integer_Matrix ("Data.Test_Y", Data.Test_Y);
--           New_Line;
         Put_Line ("Accuracy: " & Float'Image (Accuracy));
      end;
      Python_API.Py_DecRef (Estimator);
      New_Line;
   end loop;

   Python.Close_Module (Classifier);
   Python.Finalize;

   Put_Line (Routine_Name & "finished.");
   Put_Line ("----------------------------------------------");
   New_Line;

end Lesson_9A;
