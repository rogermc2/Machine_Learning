
with Ada.Containers.Vectors;
with Ada.Text_IO; use Ada.Text_IO;

with Basic_Printing; use Basic_Printing;
with ML_Arrays_And_Matrices; use ML_Arrays_And_Matrices;
with Python; use Python;

with Maths;

with ML;
with Support_5A; use Support_5A;

procedure Lesson_5A is
   type Integer3_Array is array (Integer range 1 .. 3) of Integer;

   package Integer3_Package is new
     Ada.Containers.Vectors (Positive, Integer3_Array);
   subtype Integer3_List is Integer3_Package.Vector;

   Project_Name           : constant String := "Lesson_5A ";
   Green_File_Name        : constant String := "../greenML.png";
   Forest_File_Name       : constant String := "../forest.jpg";
   Image_Data             : constant Unsigned_8_Array_3D :=
                              Get_Picture (Green_File_Name);
   Forest_Image_Data      : constant Unsigned_8_Array_3D :=
                              Get_Picture (Forest_File_Name);
   Green_Data             : constant Unsigned_8_Array_3D :=
                              Get_Pixels (Image_Data, Image_Data'First,
                                          Image_Data'Last, Image_Data'First (2),
                                          Image_Data'First (2) + 359);
   Fore_Data              : constant Unsigned_8_Array_3D :=
                              Get_Pixels (Image_Data, 31, Image_Data'Last,
                                          547, 619);
   Flat_Data              : constant Integer_Matrix :=
                              To_2D (Get_Pixels (Image_Data, Image_Data'First,
                                     Image_Data'Last, Image_Data'First (2),
                                     Image_Data'Last (2), 4));
   Yes_Length             : constant Positive := Green_Data'Length *
                              Green_Data'Length (2);
   No_Length              : constant Positive := Fore_Data'Length *
                              Fore_Data'Length (2);
   Yes_List               : constant Integer_Matrix (1 .. Yes_Length,
                                                     Green_Data'Range (3)) :=
                              To_2D (Green_Data);
   No_List                : constant Integer_Matrix (1 .. No_Length,
                                                     Fore_Data'Range (3)) :=
                              To_2D (Fore_Data);
   All_Data               : constant Integer_Matrix :=
                              Set_All_Data (Yes_List, No_List);
   Labels                 : Integer_Array (All_Data'Range);
   Py_Module              : Module;
   Seen_List              : Integer3_List;
   Pixel_Colour           : Integer3_Array;
   Weights                : Real_Float_Vector (1 .. 4);
begin
   for index in Labels'Range loop
      if index <= Yes_Length then
         Labels (index) := 1;
      else
         Labels (index) := 0;
      end if;
   end loop;

   Python.Initialize;
   Py_Module := Import_File ("lesson_5a");
   Call (Py_Module, "show_bitmap", Image_Data);
   Call (Py_Module, "show_bitmap", Green_Data);
   Call (Py_Module, "show_bitmap", Fore_Data);

   for index in Yes_List'Range loop
      for col in Pixel_Colour'Range loop
         Pixel_Colour (col) := Yes_List (index, col);
      end loop;

      if not Seen_List.Contains (Pixel_Colour) then
         Seen_List.Append (Pixel_Colour);
      end if;
   end loop;

   Put_Line (Project_Name & "Loss vs weights examples:");
   for count in 1 .. 10 loop
      for index in Weights'Range loop
         Weights (index) := Maths.Random_Float;
      end loop;
      Put ("Loss: " & Float'Image (ML.Loss (Weights, All_Data, Labels)));
      Print_Float_Vector_As_Line ("Weights", Weights);
   end loop;
   New_Line;

   --  Train the model.
   --  Result is very dependent on the initial weights
   Weights := (-2.0, 0.0, 0.093, -0.713);
   --  Lesson 5q tries different weights
   for count in 1 .. 9 loop
      for index in Weights'Range loop
         Weights (index) := Maths.Random_Float;
      end loop;
   end loop;
   ML.Fit (Weights, All_Data, Labels);
   Print_Float_Vector ("Fitted weights", Weights);

   declare
      New_Array : constant Unsigned_8_Array_3D :=
                    To_Picture (Flat_Data, Image_Data'Length,
                                Image_Data'Length (2), Weights);
   begin
      Call (Py_Module, "show_bitmap", New_Array);
      Call (Py_Module, "show_bitmap", Forest_Image_Data);
      Call (Py_Module, "show_bitmap",
            Ml.Composite (New_Array, Image_Data, Forest_Image_Data));
   end;

   Python.Finalize;

   Put_Line (Project_Name & "finished.");

end Lesson_5A;
