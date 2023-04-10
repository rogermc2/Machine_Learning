
with Ada.Text_IO; use Ada.Text_IO;

with Basic_Printing; use  Basic_Printing;
with ML_Arrays_And_Matrices; use ML_Arrays_And_Matrices;

with Support_11A; use Support_11A;

procedure Lesson_11A_Simple is
--     use Real_Float_Arrays;
   Program_Name    : constant String := "Lesson 11A Simple ";
   Test            : constant Boolean := True;
   Train_Size      : constant Positive := 5;
   Num_Clusters    : constant Positive := 3;
   Train_X         : constant Real_Float_Matrix (1 .. Train_Size,  1 .. 2) :=
                       ((1.0, 2.0),
                        (3.0, 4.0),
                        (5.0, 6.0),
                        (7.0, 8.0),
                        (9.0, 10.0));
--     Train_Y         : constant Integer_Array (1 .. Train_Size) :=
--                         (0, 2);
--     Test_X          : constant Real_Float_Matrix := Data.Test_X;
--     Test_Y          : constant Integer_Matrix := Data.Test_Y;
--     Loss            : Float;
   Best_Loss       : Float;
   Best_Centres    : constant Real_Float_Matrix :=
                       Cluster_Means (Train_X,  Num_Clusters, Best_Loss, Test);
--     Test_Center_IDs : Integer_Array (Test_X'Range);
begin
   Put_Line (Program_Name & "Best_Loss: " & Float'Image (Best_Loss));
   Print_Float_Matrix (Program_Name & "Best_Centres", Best_Centres);

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

end Lesson_11A_Simple;
