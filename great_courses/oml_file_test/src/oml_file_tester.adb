
with Ada.Text_IO; use Ada.Text_IO;

with OML_File_Tests;

procedure Oml_File_Tester is
   Routine_Name  : constant String := "Oml_File_Tester ";
begin
   Put_Line (Routine_Name);
   OML_File_Tests.Test_Data_Info;

end Oml_File_Tester;
