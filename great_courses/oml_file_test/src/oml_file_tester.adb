
with Ada.Text_IO; use Ada.Text_IO;

with OML_File_Tests;

procedure Oml_File_Tester is
   Routine_Name  : constant String := "Oml_File_Tester ";
begin
   Put_Line (Routine_Name);
   Put_Line (Routine_Name & "Test_Data_Info");
   OML_File_Tests.Test_Data_Info;
   New_Line;
   Put_Line (Routine_Name & "Test_Convert_Arff_To_Data");
   OML_File_Tests.Test_Convert_Arff_To_Data;
   New_Line;
   Put_Line (Routine_Name & "Test_Fetch_OML");
   OML_File_Tests.Test_Fetch_OML;

end Oml_File_Tester;
