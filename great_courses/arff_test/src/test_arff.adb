
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded; use  Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Text_IO.Unbounded_IO;

with Load_ARFF_Data;

procedure Test_ARFF is
   use Load_ARFF_Data;
   Routine_Name : constant String := "Test_ARFF ";
   File_Name    : constant String := "iris.arff";
   File         : File_Type;
   Data         : ARFF_Record;

begin
   Put_Line (Routine_Name);
   Load_ARFF (File_Name, Data);

end Test_ARFF;
