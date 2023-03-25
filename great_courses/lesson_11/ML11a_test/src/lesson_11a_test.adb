
with Ada.Text_IO; use Ada.Text_IO;

with Basic_Printing; use  Basic_Printing;
with ML_Arrays_And_Matrices; use ML_Arrays_And_Matrices;

with Support_11A; use Support_11A;

procedure Lesson_11A_Test is
   use Real_Float_Arrays;
   Program_Name    : constant String := "Lesson 11A Test ";
   Test            : constant Boolean := True;
   Train_Size      : constant Positive := 12;
   Num_Clusters    : constant Positive := 8;  --  k 10
   Train_X         : constant Real_Float_Matrix (1 .. Train_Size,  1 .. 10) :=
                       ((21.0, 161.0, 160.0, 185.0, 252.0, 252.0, 253.0, 193.0, 128.0, 29.0),
                        (0.0, 0.0, 0.0, 0.0, 188.0, 247.0, 65.0, 0.0, 0.0, 0.0),
                        (0.0, 19.0, 181.0, 253.0, 209.0, 88.0, 0.0, 0.0, 0.0, 0.0),
                        (0.0, 19.0, 253.0, 254.0, 213.0, 118.0, 0.0, 0.0, 0.0, 0.0),
                        (0.0, 10.0, 253.0, 151.0, 49.0, 3.0, 0.0, 0.0, 0.0, 0.0),
                        (0.0, 0.0, 3.0, 162.0, 214.0, 11.0, 0.0, 0.0, 0.0, 0.0),
                        (0.0, 1.0, 12.0, 128.0, 227.0, 253.0, 0.0, 0.0, 0.0, 0.0),
                        (0.0, 5.0, 32.0, 115.0, 239.0, 254.0, 0.0, 0.0, 0.0, 0.0),
                        (0.0, 0.0, 140.0, 253.0, 252.0, 252.0, 0.0, 0.0, 0.0, 0.0),
                        (0.0, 0.0, 94.0, 253.0, 252.0, 180.0, 0.0, 0.0, 0.0, 0.0),
                        (0.0, 20.0, 243.0, 141.0, 59.0, 13.0, 0.0, 0.0, 0.0, 0.0),
                        (0.0, 0.0, 13.0, 172.0, 204.0, 21.0, 0.0, 0.0, 0.0, 0.0));
--     Train_Y         : constant Integer_Array (1 .. Train_Size) :=
--                         (0, 2, 7, 1, 8, 6, 4, 1, 6 , 3, 2, 5);
--     Test_X          : constant Real_Float_Matrix := Data.Test_X;
--     Test_Y          : constant Integer_Matrix := Data.Test_Y;
   Loss            : Float;
   Best_Loss       : Float;
   Best_Centres    : Real_Float_Matrix :=
                       Cluster_Means (Train_X,  Num_Clusters, Best_Loss, Test);
   Centres         : Real_Float_Matrix (1 .. Num_Clusters,
                                        Train_X'Range (2));
--     Test_Center_IDs : Integer_Array (Test_X'Range);
begin
   Put_Line (Program_Name & "Best_Loss: " & Float'Image (Best_Loss));
   Print_Float_Matrix (Program_Name & "Best_Centres", Best_Centres);

   --     for rep in 1 .. 8 loop
--        for rep in 1 .. 8 loop
--           Centres := Cluster_Means (Train_X,  Num_Clusters,  Loss, Test);
--           if Loss < Best_Loss then
--              Best_Centres := Centres;
--              Best_Loss := Loss;
--           end if;
      --  Assign testing points to discovered clusters
--           Loss := Assign_Data (Test_X,  Best_Centres,  Test_Center_IDs);

      --  Use the labeled examples to label the clusters
--        end loop;

   Put_Line ("----------------------------------------------");

   Put_Line (Program_Name & "finished.");
   New_Line;

end Lesson_11A_Test;
