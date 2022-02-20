
with Ada.Directories;
with Ada.Text_IO; use Ada.Text_IO;

with Load_ARFF_Data;
with Load_ARFF_Data.ARFF_IO;
with Load_ARFF_Data.ARFF_Printing;

procedure Test_ARFF is
   use Ada.Directories;
   use Load_ARFF_Data;
   use Load_ARFF_Data.ARFF_IO;
   use Load_ARFF_Data.ARFF_Printing;
   Routine_Name : constant String := "Test_ARFF ";
   File_Name    : constant String := "../mnist_784.arff";
   Ada_File     : constant String := "../mnist_784.ada";
--     File_Name    : constant String := "../diabetes.arff";
--     Ada_File     : constant String := "../diabetes.ada";
   Data         : ARFF_Record;
begin
   Put_Line (Routine_Name);
   if Exists (Ada_File) then
      Put_Line (Routine_Name & "Reading data file " & Ada_File);
      Read_ARFF_Ada (Ada_File, Data);
      Put_Line (Routine_Name & "Data file read");
   else
      Put_Line (Routine_Name & "Loading ARFF data from " & File_Name);
      Load_ARFF (File_Name, Data);
      Put_Line (Routine_Name & "Data loaded");
      Save_ARFF (Ada_File, Data);
   end if;

   New_Line;
   Print_Description (Data);
   Put_Line ("Relation: " & Get_Relation (Data));
   Print_Attributes (Data);
   --     Print_Data (Data);
   Print_Data (Data, 1, 2);
--     Print_Data (Data, 40, 100);

end Test_ARFF;
