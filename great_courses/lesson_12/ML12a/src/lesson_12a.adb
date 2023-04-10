
with Ada.Text_IO; use Ada.Text_IO;

--  with Basic_Printing; use  Basic_Printing;
--  with ML_Arrays_And_Matrices; use ML_Arrays_And_Matrices;
with Python;

with Support_12A; use Support_12A;

procedure Lesson_12A is
--     use Real_Float_Arrays;
   Program_Name     : constant String := "Lesson 12A ";
   Vocab_Dictionary : constant Dictionary_List :=
                        Read_Vocabulary ("../../data/vocab2.txt");
   CB               : constant Data_Record :=
                        Get_Data ("../../data/cb.txt", Vocab_Dictionary);
   Classifier       : Python.Module;
begin
   Put_Line (Program_Name);

   Python.Initialize;

   Classifier := Python.Import_File ("lesson_12a");

   Python.Close_Module (Classifier);
   Python.Finalize;

   Put_Line ("----------------------------------------------");

   Put_Line (Program_Name & "finished.");
   New_Line;

end Lesson_12A;
