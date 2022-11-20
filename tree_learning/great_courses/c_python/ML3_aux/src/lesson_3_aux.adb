
with Ada.Assertions; use Ada.Assertions;
with Ada.Text_IO; use Ada.Text_IO;

with Python;

procedure Lesson_3_Aux is
   Routine_Name : constant String := "Lesson_3_Aux ";
   Errors       : Boolean;
begin
   Put_Line ("Lesson 3 Aux");

   Python.Initialize;
   Python.Execute_File ("src/word_classifier.py", Errors => Errors);
   Assert (not Errors, "Execute_File word_classifier.py failed");
   New_Line;
   Put_Line (Routine_Name & "word_classifier.py file executed");

   Python.Finalize;
   Put_Line ("----------------------------------------------");
   Put_Line (Routine_Name & "completed.");
   New_Line;

end Lesson_3_Aux;
