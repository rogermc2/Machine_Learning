
--  with Ada.Text_IO;
--  with Ada.Streams.Stream_IO; use Ada.Streams.Stream_IO;

--  with Bitmap.File_IO; use Bitmap.File_IO;
--  with Bitmap.Buffer; use Bitmap.Buffer;

with Python;
with Simple_PNG_To_BMP;

procedure Lesson_5A is
   Image_File_Name : constant String := "../greenML.png";
   --     File   : Ada.Streams.Stream_IO.File_Type;
   --     Buffer : Any_Bitmap_Buffer;
   MLP             : Python.Module;
begin
   Simple_PNG_To_BMP.Process (Image_File_Name);
   --     Open (File, In_File, "src/greenML.png");
   --     Buffer := Read_BMP_File (File);
   --     Close (File);
   Python.Initialize;
   MLP := Python.Import_File ("lesson_5a");

   Python.Call (MLP, "display", Image_File_Name);

   Python.Close_Module (MLP);
   Python.Finalize;

end Lesson_5A;
