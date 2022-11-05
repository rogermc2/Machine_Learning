
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;

with Dataset_Utilities;
with ML_Types; use ML_Types;

procedure Test_CSV_Reader is
   use Dataset_Utilities;
   use String_Package;
   Data : String_List;
   Curs : Cursor;
begin
   New_Line;
   Put_Line ("Test_CSV_Reader");
   CSV_Reader ("iris.csv", Data);

   Curs := Data.First;
   while Has_Element (Curs) loop
      Put_Line (To_String (Element (Curs)));
      Next (Curs);
   end loop;

end Test_CSV_Reader;
