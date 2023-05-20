
with Ada.Text_IO; use Ada.Text_IO;

with Basic_Printing; use  Basic_Printing;
with CSV_Data_Loader;
with ML_Arrays_And_Matrices; use ML_Arrays_And_Matrices;
with Python;
with Python_API;
with Python_CLF;

procedure Lesson_10QS is
   use CSV_Data_Loader;
   use Real_Float_Arrays;
   Program_Name  : constant String := "Lesson 10QS ";
   Dataset_Name  : constant String :=
                     "../../../neural_learning/datasets/mnist_784";
   Train_Size    : constant Positive := 5000;
   Test_Size     : constant Positive := 1000;
   Data          : constant Base_Split_State :=
                     Get_Split_State (Dataset_Name, Digits_Data, Train_Size, Test_Size,
                                      Y_Categorized => False, Normalize => False,
                                      Reload => True);
   Train_X       : constant Real_Float_Matrix := Data.Train_X;
   Train_Y       : constant Integer_Matrix := Data.Train_Y;
   Test_X        : constant Real_Float_Matrix := Data.Test_X;
   Test_Y        : constant Integer_Matrix := Data.Test_Y;
   Classifier    : Python.Module;
   Estimator     : Python_API.PyObject;
   Score         : Float;
begin
   Put_Line (Program_Name);
   --     Print_Float_Matrix ("Train X", Train_X, 21, 21, 120, 140);
   Print_Matrix_Dimensions ("Train X", Train_X);
   Print_Matrix_Dimensions ("Train Y", Train_Y);
   Print_Matrix_Dimensions ("Test X", Test_X);
   Print_Matrix_Dimensions ("Test Y", Test_Y);

   Python.Initialize;
   Classifier := Python.Import_File ("lesson_10qs");

   Estimator := Python.Call (Classifier, "init_mlp");
   Python_CLF.Call (Classifier, "fit", Estimator, Train_X, Train_Y);

   Score := Python_CLF.Call (Classifier, "score", Estimator, Test_X, Test_Y);
   Put_Line ("Multi-layer perceptron score: " & Float'Image (Score));

   Python_API.Py_DecRef (Estimator);

   Estimator := Python.Call (Classifier, "init_logistic_regression");
   Python_CLF.Call (Classifier, "fit", Estimator, Train_X, Train_Y);

   Score := Python_CLF.Call (Classifier, "score", Estimator, Test_X, Test_Y);
   Put_Line ("Logistic regression score: " & Float'Image (Score));

   Python_API.Py_DecRef (Estimator);

   Estimator := Python.Call (Classifier, "init_MultinomialNB");
   Python_CLF.Call (Classifier, "fit", Estimator, Train_X, Train_Y);

   Score := Python_CLF.Call (Classifier, "score", Estimator, Test_X, Test_Y);
   Put_Line ("Naive Bayes score: " & Float'Image (Score));

   Python_API.Py_DecRef (Estimator);

   Python.Close_Module (Classifier);
   Python.Finalize;

   Put_Line ("----------------------------------------------");

   Put_Line (Program_Name & "finished.");
   New_Line;

end Lesson_10QS;
