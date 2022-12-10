
with Ada.Text_IO; use Ada.Text_IO;

with Load_BMP_File;

procedure Lesson_5A is
   File : File_Type;
begin
   Open (File, In_File, "../greenML.png");
   null;
end Lesson_5A;
