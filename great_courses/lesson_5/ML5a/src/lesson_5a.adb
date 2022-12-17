
with Ada.Text_IO; use Ada.Text_IO;

with ML_Arrays_And_Matrices; use ML_Arrays_And_Matrices;
with Python; use Python;
with PNG_To_BMP; use PNG_To_BMP;

procedure Lesson_5A is
   Project_Name    : constant String := "Lesson_5A ";
   Image_File_Name : constant String := "../greenML.png";

   function Get_Picture (File_Name : String) return Unsigned_8_Array_3D is
      Initial : constant Unsigned_8_Array_3D :=
                  Unsigned_8_Array_3D (Process (File_Name));
      Clipped : Unsigned_8_Array_3D
        (1 .. Initial'Length - 14, Initial'Range (2), Initial'Range (3));
   begin
      for row in Clipped'Range loop
         for col in Clipped'Range (2) loop
            for pix in Clipped'Range (3) loop
               Clipped (row, col, pix) := Initial (row, col, pix);
            end loop;
         end loop;
      end loop;

      return Clipped;

   end Get_Picture;

begin
   declare
      Image_Data  : constant Unsigned_8_Array_3D :=
                      Get_Picture (Image_File_Name);
      Py_Module   : Module;
   begin
      Put_Line (Project_Name & "Image Dimensions: " &
                  Integer'Image (Image_Data'Length) & " x" &
                  Integer'Image (Image_Data'Length (2)) & " x" &
                  Integer'Image (Image_Data'Length (3)));
      Python.Initialize;
      Py_Module := Import_File ("lesson_5a");
      Python.Call (Py_Module, "show_bitmap", Image_Data);

      Python.Finalize;

   end;

end Lesson_5A;
