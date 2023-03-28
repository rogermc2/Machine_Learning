
with Ada.Text_IO; use Ada.Text_IO;

with Basic_Printing; use  Basic_Printing;
with CSV_Data_Loader;
with ML_Arrays_And_Matrices; use ML_Arrays_And_Matrices;
with ML_Types;

with Support_11A; use Support_11A;

procedure Lesson_11A is
   use CSV_Data_Loader;
   use Real_Float_Arrays;
   Program_Name     : constant String := "Lesson 11A ";
   Dataset_Name     : constant String :=
                        "../../../neural_learning/datasets/mnist_784";
   Train_Size       : constant Positive := 4700;
   Test_Size        : constant Positive := 2300;
   Num_Labelled     : constant Positive := 20;
   Num_Clusters     : constant Positive := 10;  --  k 10
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
   Labels_List      : ML_Types.Integer_List;
   Loss             : Float;
   Best_Loss        : Float;
   Best_Centres     : Real_Float_Matrix := Cluster_Means (Train_X, Num_Clusters,
                                                          Best_Loss);
   Centres          : Real_Float_Matrix (1 .. Num_Clusters, Train_X'Range (2));
   Train_Center_IDs : Integer_Array (1 .. Num_Labelled);
   Test_Center_IDs  : Integer_Array (Train_X'Range);
   Mode             : Integer;
   Sum              : Integer;
   Cluster_Labels   : Integer_Array (1 .. Num_Clusters);
   Ans              : Real_Float_List;
begin
   Put_Line (Program_Name);

   Print_Matrix_Dimensions ("Train X", Train_X);
   Print_Matrix_Dimensions ("Test X", Test_X);
   Print_Matrix_Dimensions ("Labels", Labels);

   --     Print_Integer_Matrix ("Labels", Labels);
   Put_Line (Program_Name & "Initial Loss: " & Float'Image (Best_Loss));

--     for rep in 1 .. 8 loop
   for rep in 1 .. 1 loop
      Put_Line (Program_Name & "Rep" & Integer'Image (rep) & ":");
      Centres := Cluster_Means (Train_X, Num_Clusters, Loss);
      if Loss < Best_Loss then
         Best_Centres := Centres;
         Best_Loss := Loss;
      end if;
   end loop;

   Put_Line (Program_Name & "Best_Loss: " & Float'Image (Best_Loss));
   --  Assign test points to discovered clusters
   Loss := Assign_Data (Test_X, Best_Centres, Test_Center_IDs);
   Put_Line (Program_Name & "Test Loss: " & Float'Image (Loss));

   --  Use the labeled examples to label the clusters
   Loss := Assign_Data (X_Labelled, Best_Centres, Train_Center_IDs);
   Put_Line (Program_Name & "Labelled Loss: " & Float'Image (Loss));

   Print_Integer_Array ("Train_Center_IDs", Train_Center_IDs);
   for cluster in 1 .. Num_Clusters loop
      Cluster_Labels (cluster) := Labels (1, 1);
   end loop;
   Print_Integer_Array (Program_Name & "Cluster_Labels init", Cluster_Labels);

   for cluster in 1 .. Num_Clusters loop
      Labels_List.Clear;
      for lab_index in Train_Center_IDs'Range loop
         if Train_Center_IDs (lab_index) = cluster then
            Labels_List.Append (Labels (lab_index, 1));
         end if;
      end loop;

      if not Labels_List.Is_Empty then
         Mode := Cluster_Mode (Labels_List);
         --           Put_Line (Program_Name & "Mode: " & Integer'Image (Mode));
         Cluster_Labels (cluster) := Mode;
      end if;

   end loop;

   Print_Integer_Array (Program_Name & "Cluster_Labels", Cluster_Labels);

   for cluster in 1 .. Num_Clusters loop
      Labels_List.Clear;
      for lab_index in Test_Center_IDs'Range loop
         if Test_Center_IDs (lab_index) = Test_Y (cluster, 1) then
            Labels_List.Append (Cluster_Labels (cluster));
         end if;
      end loop;

      if not Labels_List.Is_Empty then
         Sum := 0;
         for index in Labels_List.First_Index .. Labels_List.Last_Index loop
            Sum := Sum + Labels_List (index);
         end loop;
         Ans.Append (Float (Sum) / Float (Test_Y'Length));
      end if;
   end loop;
   Print_Real_Float_List (Program_Name & "Ans", Ans);

   Put_Line ("----------------------------------------------");

   Put_Line (Program_Name & "finished.");
   New_Line;

end Lesson_11A;
