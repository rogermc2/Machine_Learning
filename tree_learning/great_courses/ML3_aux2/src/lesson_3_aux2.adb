
--  with Ada.Assertions; use Ada.Assertions;
--  with Ada.Directories; use Ada.Directories;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;

with ML_Types;
with NL_Types;

with Python;

with Aux_Utils;
with Tree_Printing;
with Word_Classification; use Word_Classification;

procedure Lesson_3_Aux2 is
   use ML_Types.String_List_Package;
   Routine_Name  : constant String := "Lesson_3_Aux2 ";
   File_IE       : constant String := "ie.txt";
   File_EI       : constant String := "ei.txt";
   IE_Data       : constant ML_Types.Unbounded_List :=
                     Aux_Utils.Load_Data (File_IE);
   EI_Data       : constant ML_Types.Unbounded_List :=
                     Aux_Utils.Load_Data (File_EI);
   Data          : NL_Types.Boolean_List_2D;
   Labels        : NL_Types.Boolean_List;
   Words         : ML_Types.Bounded_String_List;
   Pronounce     : ML_Types.Bounded_String_List;
   Word_Line     : ML_Types.String_List;
   Classifier    : Python.Module;
   Test          : ML_Types.String_Multi_List;
   Test_Features : NL_Types.Boolean_List_2D;
   Test_Cursor   : Cursor;
begin
   New_Line;
   Build_Dataset (IE_Data, EI_Data, Words, Pronounce, Data, Labels);
   Put_Line (Routine_Name & "Dataset built");

   for index in Words.First_Index .. Words.Last_Index loop
      Word_Line.Clear;
      Word_Line.Append (To_Unbounded_String (Words (index)));
      Word_Line.Append (To_Unbounded_String (Pronounce (index)));
      Test.Append (Word_Line);
   end loop;

   Test_Cursor := Test.First;
   while Has_Element (Test_Cursor) loop
      Test_Features.Append (Get_Features (Element (Test_Cursor)));
      Next (Test_Cursor);
   end loop;
   Put_Line (Routine_Name & "Test_Features length: " &
               Integer'Image (Integer (Test_Features.Length)));
   Put_Line (Routine_Name & "Test_Features 1 length: " &
               Integer'Image (Integer (Test_Features.First_Element.Length)));
   Tree_Printing.Print_Unbounded_List (Routine_Name & "Feature_Names", Feature_Names);

   Python.Initialize;
   Classifier := Python.Import_File ("word_classifier_aux");
   Python.Call (Classifier, "word_classifier_aux", Data, Labels, Test_Features,
                Feature_Names);
   Put_Line (Routine_Name & "word_classifier_aux module imported");

   Python.Close_Module (Classifier);

   Put_Line ("----------------------------------------------");
   Put_Line (Routine_Name & "completed.");
   New_Line;

   Python.Finalize;

end Lesson_3_Aux2;
