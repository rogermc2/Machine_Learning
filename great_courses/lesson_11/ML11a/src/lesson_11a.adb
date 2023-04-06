
with Ada.Text_IO; use Ada.Text_IO;

with Basic_Printing; use  Basic_Printing;
with CSV_Data_Loader;
with ML_Arrays_And_Matrices; use ML_Arrays_And_Matrices;
with Python;

with Support_11A; use Support_11A;

--  This algorithm is given a set of points along with the ability to compute
--  distances between any pair of points.
--  The desired number of clusters (k) is specified.
--  Next, the algorithm searches for a set of k centers and an assignment of
--  data points to these centers.
--  The goal is to minimize the total squared distance between data points and
--  their respective centers.
--  The assignment to centers defines the clustering.

procedure Lesson_11A is
   use CSV_Data_Loader;
   use Real_Float_Arrays;
   Program_Name     : constant String := "Lesson 11A ";
   Dataset_Name     : constant String :=
                        "../../../neural_learning/datasets/mnist_784";
   Train_Size       : constant Positive := 4700;
   Test_Size        : constant Positive := 2300;
   Num_Labelled     : constant Positive := 20;   --  20
   Num_Clusters     : constant Positive := 10;   --  k 10
   Data             : constant Base_Split_State :=
                        Get_Split_State (Dataset_Name, Digits_Data, Train_Size,
                                         Test_Size, Y_Categorized => False,
                                         Normalize => False, Reload => False);
   Train_X          : constant Real_Float_Matrix := Data.Train_X;
   Train_Y          : constant Integer_Matrix := Data.Train_Y;
   Test_X           : constant Real_Float_Matrix := Data.Test_X;
   Test_Y           : constant Integer_Matrix := Data.Test_Y;
   X_Labelled       : constant Real_Float_Matrix :=
                        Slice (Train_X, 1, Num_Labelled);
   Labels           : constant Integer_Matrix :=
                        Slice (Train_Y, 1, Num_Labelled);
   Plot_Col_1       : constant Positive := 100;
   Plot_Col_2       : constant Positive := 101;
   Loss             : Float;
   Best_Loss        : Float;
   --  Best_Centres are Num_Clusters data points, each data point representing
   --  the location of the centre of a cluster
   Best_Centres     : Real_Float_Matrix :=
                        Cluster_Means (Train_X, Num_Clusters, Best_Loss);
   --  Centre_Ids associate each sample with its closest cluster centre
   Train_Center_IDs : Integer_Array (1 .. Num_Labelled);
   Test_Center_IDs  : Integer_Array (Test_X'Range);
   Cluster_Labels   : Integer_Array (1 .. Num_Clusters);
   Ans              : Real_Float_List;
   Classifier       : Python.Module;
begin
   Put_Line (Program_Name);

   Print_Matrix_Dimensions ("Train X", Train_X);
   Print_Matrix_Dimensions ("Test X", Test_X);
   Print_Matrix_Dimensions ("Labels", Labels);

   --     Print_Integer_Matrix ("Labels", Labels);
   Put_Line (Program_Name & "Initial Loss: " & Float'Image (Best_Loss));

   for rep in 1 .. 8 loop
--     for rep in 1 .. 1 loop
      Put_Line (Program_Name & "Rep" & Integer'Image (rep) & ":");
      Get_Best_Centres (Train_X, Num_Clusters, Best_Centres, Best_Loss);
   end loop;
   --  Best_Centres are Num_Clusters data points, each data point representing
   --  the location of the centre of a cluster
--     Print_Float_Matrix (Program_Name & "Final Best_Centres",
--                         Best_Centres, 1, Num_Clusters, 210, 216);
   Put_Line (Program_Name & "Best_Loss: " & Float'Image (Best_Loss));
   --  Test the clustering that was discovered.
   --  Assign test points, Test_Center_IDs, to discovered clusters
   --  Centerids is an array with one integer for each datapoint that indicates
   --  which of the centers is closest.
   --  The current loss is the minimum squared distance summed over all data
   --  points.
   --  The representational space consists of k centers and an assignment of
   --  data points to these centers.
   --  A loss function is defined as the total squared distances between the
   --  points and their respective centers.
   Loss := Assign_Data_To_Clusters (Test_X, Best_Centres, Test_Center_IDs);
--     Put_Line (Program_Name & "Test_Center_IDs length: " &
--                 Integer'Image (Test_Center_IDs'Length));
--     Print_Integer_Array ("Test_Center_IDs", Test_Center_IDs, 1, 20);
   Put_Line (Program_Name & "Test Loss: " & Float'Image (Loss));

   --  Use the labeled examples to label the clusters
   Loss := Assign_Data_To_Clusters (X_Labelled, Best_Centres, Train_Center_IDs);
   Put_Line (Program_Name & "Labelled Loss: " & Float'Image (Loss));

   Print_Integer_Array ("Train_Center_IDs", Train_Center_IDs);

   --  Cluster_Labels are generated from the modes of Labels
   Cluster_Labels := Compute_Cluster_Labels
     (Labels, Train_Center_IDs, Num_Clusters);

   Print_Integer_Array (Program_Name & "Cluster_Labels", Cluster_Labels);
   Ans := Compute_Ans (Test_Y, Test_Center_IDs, Cluster_Labels, Num_Clusters);

   Print_Real_Float_List (Program_Name & "Ans", Ans);

   Python.Initialize;

   Classifier := Python.Import_File ("lesson_11a");

   for index in 1 .. Num_Clusters loop
      Put_Line ("Cluster" & Integer'Image (index));
      Print_Matrix_Dimensions ("", Get_Plot_Data (Test_X, Cluster_Labels,
                               Test_Center_IDs, index, 1, 2));
   end loop;

--     Print_Float_Matrix ("Get_Plot_Data 1", Get_Plot_Data (Test_X,
--                         Cluster_Labels, Test_Center_IDs, 1,
--                         Plot_Col_1, Plot_Col_2));
   Print_Matrix_Dimensions ("Get_Plot_Data 1", Get_Plot_Data (Test_X,
                            Cluster_Labels, Test_Center_IDs, 1,
                            Plot_Col_1, Plot_Col_2));
   Print_Matrix_Dimensions ("Get_Plot_Data 2", Get_Plot_Data (Test_X,
                            Cluster_Labels, Test_Center_IDs, 2,
                            Plot_Col_1, Plot_Col_2));
   declare
      Cluster_Data_1 : constant Real_Float_Matrix :=
                         Get_Plot_Data (Test_X, Cluster_Labels,
                                        Test_Center_IDs, 1, Plot_Col_1,
                                        Plot_Col_2);
      Cluster_Data_2 : constant Real_Float_Matrix :=
                         Get_Plot_Data (Test_X, Cluster_Labels,
                                        Test_Center_IDs, 2, Plot_Col_1,
                                        Plot_Col_2);
   begin
      if Cluster_Data_1'Length = 0 then
         Put_Line (Program_Name & "Cluster_Data_1 is empty.");
      elsif Cluster_Data_2'Length = 0 then
         Put_Line (Program_Name & "Cluster_Data_2 is empty.");
      else
         Python.Call (Classifier, "plot", Cluster_Data_1, Cluster_Data_2);
      end if;
   end;

   Python.Close_Module (Classifier);
   Python.Finalize;

   Loss := Assign_Data_To_Clusters (Test_X, X_Labelled, Test_Center_IDs);
   Put_Line (Program_Name & "Loss: " & Float'Image (Loss));
   Put_Line ((Program_Name & "Labelled: " &
               Float'Image (Compute_Labelled
               (Train_Y, Test_Y, Test_Center_IDs))));

   Put_Line ("----------------------------------------------");

   Put_Line (Program_Name & "finished.");
   New_Line;

end Lesson_11A;
