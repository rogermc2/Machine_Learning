
with Ada.Assertions; use Ada.Assertions;
--  with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;

with GNATCOLL.Scripts;        use GNATCOLL.Scripts;
with GNATCOLL.Scripts.Python; use GNATCOLL.Scripts.Python;

procedure Lesson_3AP is
   Routine_Name  : constant String := "Lesson_3AP ";
   Repository    : constant Scripts_Repository := new Scripts_Repository_Record;
   Python        : Python_Scripting := null;
   Errors        : Boolean;
begin
   Put_Line ("Lesson 3A Python");

   Register_Python_Scripting (Repo => Repository, Module => "Test");
   --  Python_Name = "python"
   Python := Python_Scripting (Lookup_Scripting_Language (Repository,
                               Python_Name));
   Python.Execute_File ("src/load_diabetes.py", Errors => Errors);
   Assert (not Errors, "Execute_File load_diabetes.py failed");
   New_Line;
   Put_Line (Routine_Name & "load_diabetes.py file executed");

   Put_Line ("----------------------------------------------");
   Put_Line (Routine_Name & "completed.");
   New_Line;

   Python.Destroy;
   Unregister_Python_Scripting (Repository);

end Lesson_3AP;