
with Ada.Containers.Vectors;
with Ada.Text_IO; use Ada.Text_IO;

with Basic_Printing; use Basic_Printing;
with ML_Arrays_And_Matrices; use ML_Arrays_And_Matrices;
with Python; use Python;

with Support_5A; use Support_5A;

procedure Lesson_5A is
   type Integer3_Array is array (Integer range 1 .. 3) of Integer;

   package Integer3_Package is new
     Ada.Containers.Vectors (Positive, Integer3_Array);
   subtype Integer3_List is Integer3_Package.Vector;

   Project_Name    : constant String := "Lesson_5A ";
   Image_File_Name : constant String := "../greenML.png";
begin
   declare
      Image_Data            : constant Unsigned_8_Array_3D :=
                                Get_Picture (Image_File_Name);
      Green_Data            : constant Unsigned_8_Array_3D :=
                                Get_Part (Image_Data, Image_Data'First,
                                          Image_Data'Last,
                                          Image_Data'First (2),
                                          Image_Data'First (2) + 360);
      Fore_Data             : constant Unsigned_8_Array_3D :=
                                Get_Part (Image_Data, 30, Image_Data'Last,
                                          547, 620);
      Yes_Length            : constant Positive := Green_Data'Length *
                                Green_Data'Length (2);
      No_Length             : constant Positive := Fore_Data'Length *
                                Fore_Data'Length (2);
      Yes_List              : constant Integer_Matrix (1 .. Yes_Length,
                                                       Green_Data'Range (3)) :=
                                To_2D (Green_Data);
      No_List               : constant Integer_Matrix (1 .. No_Length,
                                                       Fore_Data'Range (3)) :=
                                To_2D (Fore_Data);
      All_Data              : constant Integer_Matrix := Yes_List & No_List;
      All_Data_With_Offset  :  Integer_Matrix (All_Data'Range ,
                                               1 .. All_Data'Length (2) + 1);
      Labels                : Integer_Array (All_Data'Range);
      Py_Module             : Module;
      Seen_List             : Integer3_List;
      Colour                : Integer3_Array;
   begin
      for row in All_Data_With_Offset'Range loop
         for col in All_Data_With_Offset'Range (2) loop
            if col <= All_Data'Length (2) then
               All_Data_With_Offset (row, col) := All_Data (row, col);
            else
               All_Data_With_Offset (row, col) := 1;
            end if;
         end loop;
      end loop;

      for index in Labels'Range loop
         if index <= Yes_Length then
            Labels (index) := 1;
         else
            Labels (index) := 0;
         end if;
      end loop;

      Print_Matrix_Dimensions (Project_Name & "Image", Image_Data);
      Python.Initialize;
      Py_Module := Import_File ("lesson_5a");
      Print_Matrix_Dimensions (Project_Name & "Yes_List", Yes_List);
      Python.Call (Py_Module, "show_bitmap", Image_Data);
      Python.Call (Py_Module, "show_bitmap", Green_Data);
      Python.Call (Py_Module, "show_bitmap", Fore_Data);

      for index in Yes_List'Range loop
         for col in Colour'Range loop
            Colour (col) := Yes_List (index, col);
         end loop;
         if not Seen_List.Contains (Colour) then
            Seen_List.Append (Colour);
         end if;
      end loop;
      Put_Line (Project_Name & "Seen_List length" &
                  Integer'Image (Integer (Seen_List.Length)));

      Put_Line (Project_Name & "finalizing Python");
      Python.Finalize;
      Put_Line (Project_Name & "done");

   end;

end Lesson_5A;
