--  We saw that in 4 or more dimensional spaces most things are far away.
--  As such, k nearest neighbors may have a difficult time.
--  Then we saw 1-nearest neighbor perform quite well in the 486-dimensional
--  space created for detecting malware.
--  How could that be?
--  This program checks the distances between each testing instance and its
--  nearest neighbor in the training set.
--  It compares these distances to those for a dataset with the same attribute
--  values scrambled between instances.
--  That represents a space with the same dimensionality and save distribution
--  over attribute values but where associations between attributes are random.
--  A Hstograms of the distances shows how nearest neighbor distances in the
--  real data compare to the randomized data.
--  What do you notice?
--  What impact would you expect the differences to have on the accuracy of
--  1-nearest neighbor in this dataset?

with Ada.Text_IO; use Ada.Text_IO;

--  with Basic_Printing; use Basic_Printing;
with ML_Arrays_And_Matrices; use ML_Arrays_And_Matrices;
with Neural_Utilities;
with Python;
with Python_CLF;
with Python_API;

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
   Min_Data         : constant Real_Float_List :=
                        Get_Mins (Train_Data.Features,
                                  Test_Data.Features);
   Min_Scrambled    : Real_Float_List;

   Classifier       : Python.Module;
   Estimator        : Python_API.PyObject;
   Train_Accuracy   : Float;
   Test_Accuracy    : Float;
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

   Neural_Utilities.Permute (Train_Data.Features, Train_Data.Labels);
   Min_Scrambled := Get_Mins (Train_Data.Features, Test_Data.Features);

   Python.Call (Classifier, "hist_plot", Min_Data, Min_Scrambled);
   Python.Finalize;

   Put_Line (Project_Name & "finished.");
   New_Line;

end Lesson_8QS;
