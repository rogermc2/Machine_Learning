
with Ada.Text_IO; use Ada.Text_IO;

with GID;
with Simple_PNG_To_BMP;

procedure Simple_Process_BMP is

   Image_File_Name : constant String := "../greenML.png";
begin
   Put_Line ("Simple_Process_BMP using GID version " & GID.version & " dated " & GID.reference);
   Simple_PNG_To_BMP.Process (Image_File_Name);

end Simple_Process_BMP;
