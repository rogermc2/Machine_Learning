
with Ada.Text_IO; use Ada.Text_IO;

with Basic_Printing; use  Basic_Printing;
with CSV_Data_Loader;
with ML_Arrays_And_Matrices; use ML_Arrays_And_Matrices;

with Support_11A; use Support_11A;

procedure Lesson_11A is
   use CSV_Data_Loader;
   use Real_Float_Arrays;
   Program_Name    : constant String := "Lesson 11A ";
   Dataset_Name    : constant String :=
                       "../../../neural_learning/datasets/mnist_784";
   Train_Size      : constant Positive := 4700;
   Test_Size       : constant Positive := 2300;
   Num_Labelled    : constant Positive := 20;
   Num_Clusters    : constant Positive := 10;  --  k
   Data            : constant Base_Split_State :=
                       Get_Split_State (Dataset_Name, Digits_Data, Train_Size,
                                        Test_Size, Y_Categorized => False,
                                        Normalize => False, Reload => False);
   Train_X         : constant Real_Float_Matrix := Data.Train_X;
   Train_Y         : constant Integer_Matrix := Data.Train_Y;
   Test_X          : constant Real_Float_Matrix := Data.Test_X;
   Test_Y          : constant Integer_Matrix := Data.Test_Y;
   Train_Labelled  : Real_Float_Matrix (1 .. Num_Labelled, Train_X'Range (2));
   Loss            : Float;
   Best_Loss       : Float;
   Best_Centres    : Real_Float_Matrix := Cluster_Means (Train_X, Num_Clusters,
                                                         Best_Loss);
   Centres         : Real_Float_Matrix (1 .. Num_Clusters, Train_X'Range (2));
   Test_Center_IDs : Integer_Array (Train_X'Range);
begin
   Put_Line (Program_Name);

   Print_Matrix_Dimensions ("Train X", Train_X);
   Print_Matrix_Dimensions ("Train Y", Train_Y);
   Print_Matrix_Dimensions ("Test X", Test_X);
   Print_Matrix_Dimensions ("Test Y", Test_Y);
   Print_Float_Matrix ("Train X", Train_X, 21, 21, 120, 140);

--     for rep in 1 .. 8 loop
--        Centres := Cluster_Means (Train_X, Num_Clusters, Loss);
--        if Loss < Best_Loss then
--           Best_Centres := Centres;
--           Best_Loss := Loss;
--        end if;
--        --  Assign testing points to discovered clusters
--        Loss := Assign_Data (Test_X, Best_Centres, Test_Center_IDs);
--
--        --  Use the labeled examples to label the clusters
--     end loop;

   Put_Line ("----------------------------------------------");

   Put_Line (Program_Name & "finished.");
   New_Line;

end Lesson_11A;
