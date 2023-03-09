
with Ada.Text_IO; use Ada.Text_IO;

with Basic_Printing; use Basic_Printing;
with ML_Arrays_And_Matrices; use ML_Arrays_And_Matrices;
with Python;
with Python_10A;
with Python_API;
--  with Python_CLF;
with Support_10A; use Support_10A;

procedure Lesson_10A is
   Routine_Name     : constant String := "Lesson 10A ";
   Data             : constant Data_Record := Get_Data ("../../data/ship.csv");
   S_Data           : constant Split_Data_Record := Get_Split_Data (Data);
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
   Python_10A.Call (Classifier, "fit", Estimator, S_Data.Train_Features,
                    S_Data.Train_Survived);

   declare
      --        Train_Predictions : constant Integer_Array :=
      --                              Python_10A.Call (Classifier, "predict", Estimator,
      --                                               S_Data.Train_Features);
      Test_Predictions  : constant Integer_Array :=
        Python_10A.Call (Classifier, "predict", Estimator, S_Data.Test_Features);
      --           Train_Error    : constant Float :=
      --                                 Error (Train_Predictions, Data.Train_Y);
--          Test_Error      : constant Float := Error (Test_Predictions,  Data.Test_Y);
   begin
      Print_Integer_Array ("Test Predictions", Test_Predictions, 1, 20);
      Print_Float_Vector ("Imp", Imp (Classifier, Estimator, Data));
      Put_Line ("Test_Error: " &
                  Float'Image (Error (Test_Predictions, S_Data.Test_Survived)));
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
