
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;

with ML_Arrays_And_Matrices; use ML_Arrays_And_Matrices;
with Python;
with Python_CLF;
with Python_API;

with Support_9QS; use Support_9QS;

procedure Lesson_9QS is

   Project_Name     : constant String := "Lesson 9QS ";
   Train_Data       : constant Data_Record :=
                        Get_Data ("../../data/malware-train.csv");
   Test_Data        : constant Data_Record :=
                        Get_Data ("../../data/malware-test.csv");
   Train_Size       : constant Positive := Train_Data.Features'Length;
   Test_Size        : constant Positive := Test_Data.Features'Length;
   Tests            : constant Unbounded_String_Array (1 .. 4) :=
                        (To_Unbounded_String ("linear"),
                         To_Unbounded_String ("poly"),
                         To_Unbounded_String ("sigmoid"),
                         To_Unbounded_String ("rbf"));
   Classifier       : Python.Module;
   Estimator        : Python_API.PyObject;
   Kernel           : Unbounded_String;
begin
   Put_Line ("Train_Size" & Integer'Image (Train_Size));
   Put_Line ("Test_Size" & Integer'Image (Test_Size));

   Python.Initialize;
   Classifier := Python.Import_File ("lesson_9qs");

   Estimator := Python.Call (Classifier, "init_neighbours");

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
      Put_Line ("Neighbours train accuracy: " &
                  Float'Image ((Test_Score (Train_Pred, Train_Data.Labels)) /
                    Float (Train_Size)));
      Put_Line ("Neighbours test accuracy: " &
                  Float'Image ((Test_Score (Test_Pred, Test_Data.Labels)) /
                    Float (Test_Size)));
   end;
   New_Line;

   Python_API.Py_DecRef (Estimator);

   Put_Line ("Train and test accuracies: ");
   for k in Tests'Range loop
      Kernel :=  Tests (k);
      Estimator := Python.Call (Classifier, "init_svc", Kernel);

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
         Put_Line (To_String (Kernel) & " accuracy: " &
                     Float'Image ((Test_Score (Train_Pred, Train_Data.Labels)) /
                       Float (Train_Size)) & " " &
                     Float'Image ((Test_Score (Test_Pred, Test_Data.Labels)) /
                       Float (Test_Size)));
      end;

      Python_API.Py_DecRef (Estimator);
   end loop;

   Python.Finalize;

   Put_Line (Project_Name & "finished.");
   New_Line;

end Lesson_9QS;
