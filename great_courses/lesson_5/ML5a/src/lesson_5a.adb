
with Ada.Text_IO; use Ada.Text_IO;

with Basic_Printing; use Basic_Printing;
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
      Green_Data  : constant Unsigned_8_Array_3D :=
                      Get_Part (Image_Data, Image_Data'First, Image_Data'Last,
                                Image_Data'First (2),
                                Image_Data'First (2) + 360);
      Yes_List    : constant Integer_Matrix
        (1 .. Green_Data'Length * Green_Data'Length (2),
         Green_Data'Range (3)) := To_2D (Green_Data);
      Py_Module   : Module;
   begin
      Print_Matrix_Dimensions (Project_Name & "Image", Image_Data);
      Python.Initialize;
      Py_Module := Import_File ("lesson_5a");
      Print_Matrix_Dimensions (Project_Name & "Yes_List", Yes_List);
      Python.Call (Py_Module, "show_bitmap", Image_Data);
      Python.Call (Py_Module, "show_bitmap", Green_Data);

      Put_Line (Project_Name & "finalizing Python");
      Python.Finalize;
      Put_Line (Project_Name & "done");

   end;

end Lesson_5A;
