
with Ada.Text_IO; use Ada.Text_IO;

with Basic_Printing; use Basic_Printing;
with ML_Arrays_And_Matrices; use ML_Arrays_And_Matrices;
with Python;
with Python_API;
--  with Python_CLF;
with Support_10A; use Support_10A;

procedure Lesson_10A is
   Routine_Name     : constant String := "Lesson 10A ";
   Data             : constant Split_Data_Record :=
                        Get_Split_Data ("../../data/ship.csv");
   Num_Hidden       : constant Positive := 60;
   Classifier       : Python.Module;
   Estimator        : Python_API.PyObject;
   --     Train_Error_List : Real_Float_List;
   --     Test_Error_List  : Real_Float_List;
begin
   New_Line;
   Python.Initialize;

   Classifier := Python.Import_File ("lesson_10a");

   Estimator := Python.Call (Classifier, "init_mlp", Num_Hidden);
   Call (Classifier, "fit", Estimator, Data.Train_Features, Data.Train_Labels);

   declare
      Train_Predictions : constant Real_Float_Vector :=
                            Call (Classifier, "predict", Estimator,
                                  Data.Train_Features);
      --           Test_Predictions  : constant Real_Float_Vector :=
      --                                 Python_CLF.Call (Classifier, "predict",
      --                                                  Estimator, Data.Test_X);
      --           Train_Error    : constant Float :=
      --                                 Error (Train_Predictions, Data.Train_Y);
      --           Test_Error     : constant Float :=
      --                                 Error (Test_Predictions,  Data.Test_Y);
   begin
      Print_Float_Vector ("Train_Predictions", Train_Predictions, 1, 10);
      --           Degrees.Append (degree);
      --           Train_Error_List.Append (Train_Error);
      --           Test_Error_List.Append (Test_Error);
   end;

   Python_API.Py_DecRef (Estimator);

   --     Python.Call (Classifier, "plot", Degrees, Train_Error_List,
   --                  Test_Error_List);

   Python.Close_Module (Classifier);
   Python.Finalize;

   Put_Line (Routine_Name & "finished.");
   Put_Line ("----------------------------------------------");
   New_Line;

end Lesson_10A;
