
with Ada.Text_IO; use Ada.Text_IO;

with Load_ARFF_Data;
with Load_ARFF_Data.Printing;

procedure Test_ARFF is
   use Load_ARFF_Data;
   use Load_ARFF_Data.Printing;
   Routine_Name : constant String := "Test_ARFF ";
   File_Name    : constant String := "iris.arff";
   Data         : ARFF_Record;
begin
   Put_Line (Routine_Name);
   Load_ARFF (File_Name, Data);
   Put_Line (Routine_Name & "Data loaded");
   Print_Description (Data);

end Test_ARFF;
