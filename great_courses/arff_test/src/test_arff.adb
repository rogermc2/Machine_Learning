
with Ada.Text_IO; use Ada.Text_IO;

with Load_ARFF_Data;
with Load_ARFF_Data.ARFF_IO;
with Load_ARFF_Data.ARFF_Printing;

procedure Test_ARFF is
   use Load_ARFF_Data;
   use Load_ARFF_Data.ARFF_IO;
   use Load_ARFF_Data.ARFF_Printing;
   Routine_Name : constant String := "Test_ARFF ";
   File_Name    : constant String := "../iris.arff";
   Data         : ARFF_Record;
begin
   Put_Line (Routine_Name);
   Load_ARFF (File_Name, Data);
   Put_Line (Routine_Name & "Data loaded");
   New_Line;
   Print_Description (Data);
   Put_Line ("Relation: " & Get_Relation (Data));
   Print_Attributes (Data);
   Print_Data (Data);
--     Print_Data (Data, 40, 100);
   Save_ARFF ("iris.ada", Data);

end Test_ARFF;
