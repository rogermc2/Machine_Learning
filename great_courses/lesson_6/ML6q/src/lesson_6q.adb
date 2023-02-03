
with Ada.Text_IO; use Ada.Text_IO;

--  with Base;
--  with Base_Neural;
with CSV_Data_Loader;
--  with Basic_Printing; use Basic_Printing;
with ML_Arrays_And_Matrices; use ML_Arrays_And_Matrices;
--  with ML_Types;
--  with Neural_Utilities;
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
                                       Reload => False);
   Train_X        : constant Real_Float_Matrix := Data.Train_X;
   Train_Y        : constant Integer_Array := To_Integer_Array (Data.Train_Y);
   Test_X         : constant Real_Float_Matrix := Data.Test_X;
   Test_Y         : constant Integer_Array := To_Integer_Array (Data.Test_Y);
   Classifier     : Module;
   CLF            : Python_API.PyObject;
   Score          : Float;

   --     procedure Do_Predictions is
   --        Predictions : constant Integer_Array := Python_CLF.Call
   --          (Classifier, "predict", CLF, Train_X);
   --        Correct     : Natural := 0;
   --        Accuracy    : Float;
   --     begin
   --        for index in Predictions'Range loop
   --           if Predictions (index) = Test_Y (index) then
   --              Correct := Correct + 1;
   --           end if;
   --        end loop;
   --
   --        Accuracy := Float (Correct) / Float (Predictions'Length);
   --        Put_Line ("Accuracy: " & Float'Image (Accuracy));
   --
   --     end Do_Predictions;

   --  -------------------------------------------------------------------------

begin
   Python.Initialize;
   Classifier := Import_File ("lesson_6q");
   CLF := Python_CLF.Call (Classifier, "init_mlpclassifer", 250, 10000);
   Python_CLF.Call (Classifier, "fit", CLF, Train_X, Train_Y);
   Score := Python_CLF.Call (Classifier, "score", CLF, Test_X, Test_Y);

   Put_Line (Project_Name & "Score: " & Float'Image (Score));
   --     Do_Predictions;

   Python.Finalize;

   Put_Line (Project_Name & "finished.");
   New_Line;

end Lesson_6Q;
