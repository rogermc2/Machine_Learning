
with Ada.Text_IO; use Ada.Text_IO;

with ML_Arrays_And_Matrices; use ML_Arrays_And_Matrices;
with Python;
with Python_CLF;
with Python_API;
with Shuffler;

with Support_8QS; use Support_8QS;

procedure Lesson_8QS is
   Project_Name     : constant String := "Lesson 8QS ";
   Num_Samples      : constant := 4000;
   Num_Neighbours   : constant Positive := 1;
   Train_Data       : Data_Record :=
     Get_Data ("../../data/malware-train.csv", Num_Samples);
   Test_Data        : constant Data_Record :=
     Get_Data ("../../data/malware-test.csv");
   Train_Size       : constant Positive := Train_Data.Features'Length;
   Test_Size        : constant Positive := Test_Data.Features'Length;
   Min_Data         : Real_Float_List := Get_Mins (Train_Data.Features,
                                                   Test_Data.Features);

   Classifier       : Python.Module;
   Estimator        : Python_API.PyObject;
   Train_Accuracy   : Float;
   Test_Accuracy    : Float;
   Accuracy_2D      : Real_Float_List_2D;
begin
   Python.Initialize;
   Classifier := Python.Import_File ("lesson_8qs");

   Estimator :=
     Python.Call (Classifier, "init_NeighborsClassifier", Num_Neighbours);
   Python_CLF.Call (Classifier, "fit", Estimator, Train_Data.Features,
                    Train_Data.Labels);
   declare
      Train_Pred : constant Real_Float_Vector :=
        Python_CLF.Call (Classifier, "predict", Estimator,
                         Train_Data.Features);
      Test_Pred  : constant Real_Float_Vector :=
        Python_CLF.Call (Classifier, "predict", Estimator,
                         Test_Data.Features);
   begin
      Train_Accuracy := Float (Test_Score (Train_Pred, Train_Data.Labels)) /
                               Float (Train_Size);
      Test_Accuracy := Float (Test_Score (Test_Pred, Test_Data.Labels)) /
        Float (Test_Size);
      Put_Line ("Train Accuracy: " & Float'Image (Train_Accuracy));
      Put_Line ("Test Accuracy: " & Float'Image (Test_Accuracy));
   end;

   Python_API.Py_DecRef (Estimator);

   Shuffler.Shuffle (Train_Data.Features, Train_Data.Labels);

   Python.Call (Classifier, "plot", Accuracy_2D);
   Python.Finalize;

   Put_Line (Project_Name & "finished.");
   New_Line;

end Lesson_8QS;
