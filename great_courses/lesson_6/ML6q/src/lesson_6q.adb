
with Ada.Text_IO; use Ada.Text_IO;

with CSV_Data_Loader;
--  with Basic_Printing; use Basic_Printing;
with ML_Arrays_And_Matrices; use ML_Arrays_And_Matrices;
with Python; use Python;
with Python_API;
with Python_CLF;

procedure Lesson_6Q is
   use CSV_Data_Loader;

   function To_Integer_Array (Data : Integer_Matrix) return Integer_Array is
      Array_1D : Integer_Array (Data'Range);
   begin
      for row in Data'Range loop
         Array_1D (row) := Data (row, 1);
      end loop;

      return Array_1D;

   end To_Integer_Array;

   Project_Name   : constant String := "Lesson 6Q ";
   Data_File_Name : constant String :=
                      "../../../neural_learning/datasets/mnist_784";
   Train_Size     : constant Positive := 5000;
   Test_Size      : constant Positive := 10000;
   Data           : constant Base_Split_State :=
                      Get_Split_State (Data_File_Name, Digits_Data, Train_Size,
                                       Test_Size, Y_Categorized => False,
                                       Normalize => True, Reload => True);
   Train_X        : constant Real_Float_Matrix := Data.Train_X;
   Train_Y        : constant Integer_Array := To_Integer_Array (Data.Train_Y);
   Test_X         : constant Real_Float_Matrix := Data.Test_X;
   Test_Y         : constant Integer_Array := To_Integer_Array (Data.Test_Y);
   Classifier     : Module;
   CLF            : Python_API.PyObject;
   Score          : Float;
begin
   Python.Initialize;
   Classifier := Import_File ("lesson_6q");
   CLF := Python_CLF.Call (Classifier, "init_mlpclassifer", 250, 10000);
   Python_CLF.Call (Classifier, "fit", CLF, Train_X, Train_Y);
   Score := Python_CLF.Call (Classifier, "score", CLF, Test_X, Test_Y);

   Put_Line (Project_Name & "MLP classifer score: " & Float'Image (Score));

   CLF := Python_CLF.Call (Classifier, "init_classifer", 250);
   Python_CLF.Call (Classifier, "fit", CLF, Train_X, Train_Y);
   Score := Python_CLF.Call (Classifier, "score", CLF, Test_X, Test_Y);

   Put_Line (Project_Name & "Decision tree classifier score: " &
               Float'Image (Score));

   Python_API.Py_DecRef (CLF);
   CLF := Python_CLF.Call (Classifier, "multinomial_fit", Train_X, Train_Y);
   Score := Python_CLF.Call (Classifier, "score", CLF, Test_X, Test_Y);

   Put_Line (Project_Name & "Multinomial classifier score: " &
               Float'Image (Score));

   Python.Finalize;

   Put_Line (Project_Name & "finished.");
   New_Line;

end Lesson_6Q;
