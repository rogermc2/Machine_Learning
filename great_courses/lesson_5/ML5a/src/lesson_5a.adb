
with Ada.Text_IO; use Ada.Text_IO;

with Simple_PNG_To_BMP; use Simple_PNG_To_BMP;

procedure Lesson_5A is
   Project_Name    : constant String := "Lesson_5A ";
   Image_File_Name : constant String := "../greenML.png";
begin
   declare
      Image_Data : constant Simple_PNG_To_BMP.Image_Array := Process (Image_File_Name);
   begin
      Put_Line (Project_Name & "Image Dimensions: " &
                  Integer'Image (Height (Image_Data)) & " x" & Integer'Image (Width (Image_Data)));
   end;

end Lesson_5A;
