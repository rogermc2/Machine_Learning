
with Ada.Exceptions; use Ada.Exceptions;
with Ada.Text_IO; use Ada.Text_IO;

with Basic_Printing; use  Basic_Printing;
with Python;

with Support_16A; use Support_16A;

procedure Lesson_16A is
   Program_Name     : constant String := "Lesson 16A ";
--     Vocab_Dictionary : constant Dictionary_List :=
--       Read_Vocabulary ("../../data/vocab2.txt");
--     Glove_Data          : constant Data_Items :=
--       Get_Data ("../../data/glove.6B/glove.6B.100d.txt");
   Newsgroups_File  : constant String := "Newsgroups.data";
   Classifier       : Python.Module;
   Newsgroups       : Newsgroups_Record;
begin
   Put_Line (Program_Name);
   Python.Initialize;
   Classifier := Python.Import_File ("lesson_16a");

   Newsgroups := Load_Data (Classifier, Newsgroups_File, True);

   Put_Line (Program_Name  & "Newsgroups file read");
   Put_Line (Program_Name  & "Newsgroups.Data length" &
               Integer'Image (Integer (Newsgroups.Data.Length)));
--     Print_Unbound_List (Program_Name & "Newsgroups.Data", Newsgroups.Data);
   New_Line;

--     Python.Call (Classifier, "plot", Alphas, Result);

   Python.Close_Module (Classifier);
   Python.Finalize;

   Put_Line ("----------------------------------------------");

   Put_Line (Program_Name & "finished.");
   New_Line;

exception
   when Error: Constraint_Error => Put_Line (Program_Name &
                                               "Constraint_Error");
      Put_Line (Exception_Information(Error));
   when Error: others => Put_Line (Program_Name & "exception");
      Put_Line (Exception_Information(Error));

end Lesson_16A;
