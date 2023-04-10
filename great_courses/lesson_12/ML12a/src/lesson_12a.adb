
with Ada.Text_IO; use Ada.Text_IO;

--  with Basic_Printing; use  Basic_Printing;
with ML_Arrays_And_Matrices; use ML_Arrays_And_Matrices;
with Python;

with Support_12A; use Support_12A;

procedure Lesson_12A is
   use Real_Float_Arrays;
   Program_Name     : constant String := "Lesson 12A ";
   Vocab            : constant String := "../../../data/vocab2.txt";
   CB               : constant String := "../../../data/cb.txt";
   Classifier       : Python.Module;
begin
   Put_Line (Program_Name);

   Python.Initialize;

   Classifier := Python.Import_File ("lesson_11a");

   Python.Close_Module (Classifier);
   Python.Finalize;

   Put_Line ("----------------------------------------------");

   Put_Line (Program_Name & "finished.");
   New_Line;

end Lesson_12A;
