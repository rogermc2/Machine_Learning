
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
--     Test_Y           : constant Integer_Matrix := Data.Test_Y;
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
   Train_Center_IDs : Integer_Array (Test_X'Range);
   Test_Center_IDs  : Integer_Array (Train_X'Range);
   Mode             : Integer;
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

      Put_Line (Program_Name & "Best_Loss: " & Float'Image (Best_Loss));
      --  Assign test points to discovered clusters
      Loss := Assign_Data (Test_X, Best_Centres, Test_Center_IDs);
      Put_Line (Program_Name & "Test Loss: " & Float'Image (Loss));

      --  Use the labeled examples to label the clusters
      Loss := Assign_Data (X_Labelled, Best_Centres, Train_Center_IDs);
      Put_Line (Program_Name & "Labelled Loss: " & Float'Image (Loss));

      for index in 1 .. Num_Clusters loop
         for lab_index in Train_Center_IDs'Range loop
            if Train_Center_IDs (lab_index) = index then
               Labels_List.Append (Labels (lab_index, 1));
            end if;
         end loop;
         Mode := Cluster_Mode (Labels_List);
         Put_Line (Program_Name & "Mode: " & Integer'Image (Mode));

      end loop;

   end loop;

   Put_Line ("----------------------------------------------");

   Put_Line (Program_Name & "finished.");
   New_Line;

end Lesson_11A;
