
--  with Ada.Assertions; use Ada.Assertions;
with Ada.Text_IO; use Ada.Text_IO;

with Python;

procedure Lesson_3_Aux is
   Routine_Name : constant String := "Lesson_3_Aux ";
   Classifier   : Python.Module;
begin
   Put_Line ("Lesson 3 Aux");

   Python.Initialize;
   Classifier := Python.Import_File ("word_classifier.py");
   New_Line;
   Put_Line (Routine_Name & "word_classifier.py file executed");

   Python.Finalize;
   Put_Line ("----------------------------------------------");
   Put_Line (Routine_Name & "completed.");
   New_Line;

end Lesson_3_Aux;
