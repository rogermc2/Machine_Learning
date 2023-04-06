
with Ada.Text_IO; use Ada.Text_IO;

with Basic_Printing; use  Basic_Printing;
with CSV_Data_Loader;
with ML_Arrays_And_Matrices; use ML_Arrays_And_Matrices;
with Python;

--  with Support_11A; use Support_11A;
with Support_11QS; use Support_11QS;

--  This algorithm is given a set of points along with the ability to compute
--  distances between any pair of points.
--  The desired number of clusters (k) is specified.
--  Next, the algorithm searches for a set of k centers and an assignment of
--  data points to these centers.
--  The goal is to minimize the total squared distance between data points and
--  their respective centers.
--  The assignment to centers defines the clustering.

procedure Lesson_11QS is
   use CSV_Data_Loader;
   use Real_Float_Arrays;
   Program_Name     : constant String := "Lesson 11QS ";
   Dataset_Name     : constant String :=
                        "../../../neural_learning/datasets/mnist_784";
   Train_Size       : constant Positive := 4700;
   Test_Size        : constant Positive := 2300;
   Num_Clusters     : constant Positive := 10;   --  k 10
   Data             : constant Base_Split_State :=
                        Get_Split_State (Dataset_Name, Digits_Data, Train_Size,
                                         Test_Size, Y_Categorized => False,
                                         Normalize => False, Reload => False);
   Train_X          : constant Real_Float_Matrix := Data.Train_X;
   Train_Y          : constant Integer_Matrix := Data.Train_Y;
   Test_X           : constant Real_Float_Matrix := Data.Test_X;
--     Test_Y           : constant Integer_Matrix := Data.Test_Y;

   Classifier       : Python.Module;
   Loss             : Float;
begin
   Put_Line (Program_Name);

   Print_Matrix_Dimensions ("Train X", Train_X);
   Print_Matrix_Dimensions ("Test X", Test_X);

   Python.Initialize;

   Classifier := Python.Import_File ("lesson_11qs");
   Loss := Try_Clusterer (Classifier, Num_Clusters, Train_X, Train_Y);

   Python.Close_Module (Classifier);
   Python.Finalize;

   Put_Line (Program_Name & "Loss: " & Float'Image (Loss));

   Put_Line ("----------------------------------------------");

   Put_Line (Program_Name & "finished.");
   New_Line;

end Lesson_11QS;