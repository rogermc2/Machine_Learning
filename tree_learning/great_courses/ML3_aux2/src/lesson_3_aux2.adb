
with Ada.Assertions; use Ada.Assertions;
--  with Ada.Directories; use Ada.Directories;
--  with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;

with GNATCOLL.Scripts;        use GNATCOLL.Scripts;
with GNATCOLL.Scripts.Python; use GNATCOLL.Scripts.Python;

with ML_Types;
with NL_Types;

with Aux_Utils;
with Word_Classification; use Word_Classification;

procedure Lesson_3_Aux2 is
   Routine_Name : constant String := "Lesson_3_Aux2 ";
   Repository   : constant Scripts_Repository := new Scripts_Repository_Record;
   Python       : Python_Scripting := null;
   File_IE      : constant String := "ie.txt";
   File_EI      : constant String := "ei.txt";
   IE_Data      : constant ML_Types.Unbounded_List :=
                    Aux_Utils.Load_Data (File_IE);
   EI_Data      : constant ML_Types.Unbounded_List :=
                    Aux_Utils.Load_Data (File_EI);
   Labels       : ML_Types.Unbounded_List;
   Words        : ML_Types.Unbounded_List;
   Pronounce    : ML_Types.Unbounded_List;
   Data         : NL_Types.Boolean_List_2D;
   Errors       : Boolean;
begin
   Put_Line ("Lesson 3 Aux2");
   Build_Dataset (IE_Data, EI_Data, Labels, Words, Pronounce, Data);

   Register_Python_Scripting (Repo => Repository, Module => "Test");
   --  Python_Name = "python"
   Python := Python_Scripting (Lookup_Scripting_Language (Repository,
                               Python_Name));
--     Put_Line (Routine_Name & "Current_Directory: " & Current_Directory);
   Python.Execute_File ("src/word_classifier_aux.py", Errors => Errors);
   Assert (not Errors, "Execute_File word_classifier_aux.py failed");
   New_Line;
   Put_Line (Routine_Name & "word_classifier_aux.py file executed");

   Put_Line ("----------------------------------------------");
   Put_Line (Routine_Name & "completed.");
   New_Line;

   Python.Destroy;
   Unregister_Python_Scripting (Repository);

end Lesson_3_Aux2;
