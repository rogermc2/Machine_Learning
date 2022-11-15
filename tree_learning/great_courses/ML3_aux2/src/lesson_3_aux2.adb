
with Ada.Assertions; use Ada.Assertions;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;

with GNATCOLL.Scripts;        use GNATCOLL.Scripts;
with GNATCOLL.Scripts.Python; use GNATCOLL.Scripts.Python;

with ML_Types;

with Aux_Utils;

procedure Lesson_3_Aux2 is
   Routine_Name : constant String := "Lesson_3_Aux2 ";
   Repository   : constant Scripts_Repository := new Scripts_Repository_Record;
   Python       : Python_Scripting := null;
   File_IE      : constant String := "ie.txt";
   File_EI      : constant String := "ei.txt";
   File_ID      : File_Type;
   IE_Data      : ML_Types.Unbounded_List := Aux_Utils.Load_Data (File_IE);
   EI_Data      : ML_Types.Unbounded_List := Aux_Utils.Load_Data (File_EI);
   Errors       : Boolean;
begin
   Put_Line ("Lesson 3 Aux2");


   Register_Python_Scripting (Repo => Repository, Module => "Test");
   --  Python_Name = "python"
   Python := Python_Scripting (Lookup_Scripting_Language (Repository,
                               Python_Name));
   Python.Execute_File ("src/word_classifier.py", Errors => Errors);
   Assert (not Errors, "Execute_File word_classifier.py failed");
   New_Line;
   Put_Line (Routine_Name & "word_classifier.py file executed");

   Put_Line ("----------------------------------------------");
   Put_Line (Routine_Name & "completed.");
   New_Line;

   Python.Destroy;
   Unregister_Python_Scripting (Repository);

end Lesson_3_Aux2;
