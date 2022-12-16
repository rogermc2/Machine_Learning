
with Ada.Text_IO; use Ada.Text_IO;

with ML_Arrays_And_Matrices; use ML_Arrays_And_Matrices;
with Python; use Python;
with Simple_PNG_To_BMP; use Simple_PNG_To_BMP;

procedure Lesson_5A is
   Project_Name    : constant String := "Lesson_5A ";
   Image_File_Name : constant String := "../greenML.png";
begin
   declare
      Image_Data : constant Integer_Matrix :=
                     Integer_Matrix (As_Matrix (Process (Image_File_Name)));
      Py_Module  : Module;
   begin
      Put_Line (Project_Name & "Image Dimensions: " &
                  Integer'Image (Image_Data'Length) & " x" &
                  Integer'Image (Image_Data'Length (2)));
      Python.Initialize;
      Py_Module := Import_File ("lesson_5a");
--        Python.Execute_String ("import matplotlib.pyplot as plt");
      Python.Call (Py_Module, "show", Image_Data);

      Python.Finalize;

   end;

end Lesson_5A;
