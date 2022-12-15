
--  with Ada.Text_IO;
with Simple_PNG_To_BMP;

procedure Lesson_5A is
   Image_File_Name : constant String := "../greenML.png";
begin
   declare
      File_Name : String := Simple_PNG_To_BMP.Process (Image_File_Name);
   begin
      null;
   end;

end Lesson_5A;
