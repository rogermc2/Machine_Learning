
with Ada.Text_IO; use Ada.Text_IO;

--  with Basic_Printing; use Basic_Printing;
with CSV_Data_Loader;
with ML_Arrays_And_Matrices; use ML_Arrays_And_Matrices;
with ML_Types;
with Python;
with Python_API;
with Python_CLF;
with Support_9AUX; use Support_9AUX;

procedure Lesson_9AUX is
   use CSV_Data_Loader;
   Routine_Name     : constant String := "Lesson Lesson_9AUX ";
   Train_Size       : constant Positive := 500;
   Test_Size        : constant Positive := 200;
   Data             : constant Base_Split_State := Get_Split_State
     ("../../data/diabetes.csv", Diabetes_Data, Train_Size,
      Test_Size, Shuffle => True, Reload => True);

   Classifier       : Python.Module;
   Estimator        : Python_API.PyObject;
   Train_Error_List : Real_Float_List;
--     Test_Error_List  : Real_Float_List;
   Leaves           : ML_Types.Integer_List;
begin
   New_Line;
   Python.Initialize;

   Classifier := Python.Import_File ("lesson_9aux");

   for nodes in 2 .. 30 loop
      Estimator := Python.Call (Classifier, "init_tree", nodes);
      Python_CLF.Call (Classifier, "fit", Estimator, Data.Train_X,
                       Data.Train_Y);

      declare
         Train_Predictions : constant Real_Float_Vector :=
                               Python_CLF.Call (Classifier, "predict",
                                                Estimator, Data.Train_X);
--           Test_Predictions  : constant Real_Float_Vector :=
--                                 Python_CLF.Call (Classifier, "predict",
--                                                  Estimator, Data.Test_X);
         Train_Error       : constant Float :=
                               Error (Train_Predictions, Data.Train_Y);
--           Test_Error        : constant Float :=
--                                 Error (Test_Predictions,  Data.Test_Y);
      begin
         Leaves.Append (nodes);
         Train_Error_List.Append (Train_Error);
--           Test_Error_List.Append (Test_Error);
      end;

      Python_API.Py_DecRef (Estimator);
      New_Line;
   end loop;

   Python.Call (Classifier, "plot", Leaves, Train_Error_List,
                Train_Error_List);

   Python.Close_Module (Classifier);
   Python.Finalize;

   Put_Line (Routine_Name & "finished.");
   Put_Line ("----------------------------------------------");
   New_Line;

end Lesson_9AUX;
