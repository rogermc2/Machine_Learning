
--  with Ada.Text_IO; use Ada.Text_IO;
with Ada.Streams.Stream_IO; use Ada.Streams.Stream_IO;

with Bitmap.File_IO; use Bitmap.File_IO;
with Bitmap.Buffer; use Bitmap.Buffer;

procedure Lesson_5A is
   File   : Ada.Streams.Stream_IO.File_Type;
   Buffer : Any_Bitmap_Buffer;
begin
   Open (File, In_File, "../greenML.png");
   Buffer := Read_BMP_File (File);

   Close (File);

end Lesson_5A;
