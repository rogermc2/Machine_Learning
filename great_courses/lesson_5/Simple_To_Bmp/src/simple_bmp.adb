
with Ada.Text_IO; use Ada.Text_IO;

with GID;
with Simple_Support; use Simple_Support;

procedure Simple_BMP is

   Image_File_Name : constant String := "../greenML.png";
begin
   Put_Line ("Simple_BMP using GID version " & GID.version & " dated " & GID.reference);
   Process (Image_File_Name);

end Simple_BMP;
