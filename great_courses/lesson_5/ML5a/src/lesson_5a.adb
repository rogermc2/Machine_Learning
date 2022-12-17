
with Ada.Text_IO; use Ada.Text_IO;

with ML_Arrays_And_Matrices; use ML_Arrays_And_Matrices;
with Python; use Python;

with Support_5A; use Support_5A;

procedure Lesson_5A is
   Project_Name    : constant String := "Lesson_5A ";
   Image_File_Name : constant String := "../greenML.png";
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
