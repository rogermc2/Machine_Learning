
--  with Ada.Assertions; use Ada.Assertions;
with Ada.Text_IO; use Ada.Text_IO;

with Maths;

--  with Basic_Printing; use Basic_Printing;
with ML_Arrays_And_Matrices; use ML_Arrays_And_Matrices;
with Process; use Process;
with Python;
with Support_8Aux; use Support_8Aux;

procedure Lesson_8Aux is
   Project_Name     : constant String := "Lesson 8Aux ";
   Num_Samples      : constant Positive := 10500;
   Test_Size        : constant Positive := 10000;
   Train_Size       : constant Positive := Num_Samples - Test_Size;
   Max_Leaf_Nodes   : constant Positive := 7;
   Scale            : constant Positive := 7;
   All_Data         : Real_Float_Matrix (1 .. Num_Samples, 1 .. 2);
   Labs             : Boolean_Array (1 .. Num_Samples);
   Data             : Real_Float_Matrix (1 .. Train_Size, 1 .. 2);
   Scaled_Data      : Real_Float_Matrix (1 .. Scale * Num_Samples,
                                         Data'Range (2));
   Test_Data        : Real_Float_Matrix (1 .. Test_Size, 1 .. 2);
   Train_Labs       : Boolean_Array (1 .. Train_Size);
   Scaled_Labs      : Boolean_Array (Scaled_Data'Range);
   Test_Labs        : Boolean_Array (1 .. Test_Size);
   Classifier       : Python.Module;
   Comfy            : Real_Vector_List;
   Uncomfy          : Real_Vector_List;
begin
   for row in All_Data'Range loop
      All_Data (row, 1) := 65.0 + 12.0 * abs (Maths.Random_Float);
      All_Data (row, 2) := 15.0 + 75.0 * abs (Maths.Random_Float);
   end loop;

   for row in Labs'Range loop
      Labs (row) := Comfort (All_Data (row, 1), All_Data (row, 2));
   end loop;

   for row in All_Data'Range loop
      if Labs (row) then
         Comfy.Append ((All_Data  (row, 1), All_Data  (row, 2)));
      else
         Uncomfy.Append ((All_Data  (row, 1), All_Data  (row, 2)));
      end if;
   end loop;

   Train_Test_Split
     (X          => All_Data,   Y         => Labs,
      Train_Size => Train_Size, Test_Size => Test_Size,
      Train_X    => Data,       Train_Y   => Train_Labs,
      Test_X     => Test_Data,  Test_Y    => Test_Labs);

   Python.Initialize;
   Classifier := Python.Import_File ("lesson_8aux");

   Python.Call (Classifier, "xy_plot", Comfy, Uncomfy);
   Process_Decision_Tree (Classifier, Data, Test_Data, Train_Labs, Test_Labs,
                          Max_Leaf_Nodes);

  Process_Neighbours (Classifier, Data, Test_Data , Train_Labs, Test_Labs, 1);

   for row in Scaled_Data'Range loop
      Scaled_Data (row, 1) := 65.0 + 12.0 * abs (Maths.Random_Float);
      Scaled_Data (row, 2) := 15.0 + 75.0 * abs (Maths.Random_Float);
   end loop;

   for row in Scaled_Labs'Range loop
      Scaled_Labs (row) := Comfort (Scaled_Data (row, 1),
                                    Scaled_Data (row, 2));
   end loop;

   New_Line;
   Put_Line (Project_Name & "Scaled_Data test.");
   Process_Neighbours (Classifier, Scaled_Data, Test_Data , Scaled_Labs,
                       Test_Labs, 1);

   Python.Finalize;

   Put_Line (Project_Name & "finished.");
   New_Line;

end Lesson_8Aux;
