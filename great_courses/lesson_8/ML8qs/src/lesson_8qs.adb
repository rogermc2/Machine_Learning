
with Ada.Text_IO; use Ada.Text_IO;

with ML_Arrays_And_Matrices; use ML_Arrays_And_Matrices;
with Python;
with Python_CLF;
with Python_API;

with Support_8QS; use Support_8QS;

procedure Lesson_8QS is
   Project_Name     : constant String := "Lesson 8QS ";
   Num_Samples      : constant := 4000;
   Train_Data       : constant Data_Record :=
                        Get_Data ("../../data/malware-train.csv", Num_Samples);
   Test_Data        : constant Data_Record :=
     Get_Data ("../../data/malware-test.csv", Num_Samples);
   Train_Size       : constant Positive := Num_Samples;
   Test_Size        : constant Positive := Test_Data.Features'Length;
   Tests            : constant Integer_Array (1 .. 4) := (1, 5, 7,9);
   Num_Neighbours   : Positive;
   Classifier       : Python.Module;
   Estimator        : Python_API.PyObject;
   Accuracy         : Real_Float_List;
   Accuracy_2D      : Real_Float_List_2D;
begin

   Python.Initialize;
   Classifier := Python.Import_File ("lesson_8qs");

   for k in Tests'Range loop
      Num_Neighbours := Tests (k);
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
         Accuracy.Clear;
         Accuracy.Append (Float (Num_Neighbours));
         Accuracy.Append (Float (Test_Score (Train_Pred, Train_Data.Labels)) /
                            Float (Train_Size));
         Accuracy.Append (Float (Test_Score (Test_Pred, Test_Data.Labels)) /
                            Float (Test_Size));
      end;
      Python_API.Py_DecRef (Estimator);

      Accuracy_2D.Append (Accuracy);
   end loop;

   Python.Call (Classifier, "plot", Accuracy_2D);
   Python.Finalize;

   Put_Line (Project_Name & "finished.");
   New_Line;

end Lesson_8QS;
