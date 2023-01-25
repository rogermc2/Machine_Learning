
with Ada.Containers.Vectors;
with Ada.Text_IO; use Ada.Text_IO;

with Maths;

with Basic_Printing; use Basic_Printing;
with ML_Arrays_And_Matrices; use ML_Arrays_And_Matrices;
with Python; use Python;

with ML; use ML;
with Support_5A; use Support_5A;

procedure Lesson_5A is
   use Real_Float_Arrays;
   type Integer3_Array is array (Integer range 1 .. 3) of Integer;

   package Integer3_Package is new
     Ada.Containers.Vectors (Positive, Integer3_Array);
   subtype Integer3_List is Integer3_Package.Vector;

   Project_Name          : constant String := "Lesson_5A ";
   Image_File_Name       : constant String := "../greenML.png";
   Image_Data            : constant Unsigned_8_Array_3D :=
                             Get_Picture (Image_File_Name);
   Green_Data            : constant Unsigned_8_Array_3D :=
                             Get_Pixels (Image_Data, Image_Data'First,
                                         Image_Data'Last, Image_Data'First (2),
                                         Image_Data'First (2) + 359);
   Fore_Data             : constant Unsigned_8_Array_3D :=
                             Get_Pixels (Image_Data, 31, Image_Data'Last,
                                         547, 619);
   Flat_Data             : constant Integer_Matrix :=
                             To_2D (Get_Pixels (Image_Data, Image_Data'First,
                                    Image_Data'Last, Image_Data'First (2),
                                    Image_Data'Last (2), 4));
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
   All_Data              : constant Integer_Matrix :=
                             Set_All_Data (Yes_List, No_List);
   Labels                : Integer_Array (All_Data'Range);
   Py_Module             : Module;
   Seen_List             : Integer3_List;
   Colour                : Integer3_Array;
   Weights               : Real_Float_Vector (1 .. 4);
begin
   for index in Labels'Range loop
      if index <= Yes_Length then
         Labels (index) := 1;
      else
         Labels (index) := 0;
      end if;
   end loop;
   Print_Matrix_Dimensions (Project_Name & "Yes_List", Yes_List);
   Print_Matrix_Dimensions (Project_Name & "No_List", No_List);
   Print_Matrix_Dimensions (Project_Name & "All_Data", All_Data);

   Print_Matrix_Dimensions (Project_Name & "Image", Image_Data);
   Python.Initialize;
   Py_Module := Import_File ("lesson_5a");
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

   Put_Line (Project_Name & "Loss vs weights examples:");
   for count in 1 .. 10 loop
      for index in Weights'Range loop
         Weights (index) := Maths.Random_Float;
      end loop;
      Put ("Loss: " & Float'Image (Loss (Weights, All_Data, Labels)));
      Print_Float_Vector_As_Line ("Weights", Weights);
   end loop;
   New_Line;

   --  Train the model by using a fit function to fit the model to the data.
   --  The weights will be updated by gradient descent.
   Weights := (0.786,  0.175, -0.558, -0.437);
   Fit (Weights, All_Data, Labels);
   Print_Float_Vector ("Fitted weights", Weights);

   Python.Call (Py_Module, "show_bitmap",
                To_Picture (Flat_Data, Image_Data'Length, Image_Data'Length (2),
                  Weights));
   Python.Finalize;

   Put_Line (Project_Name & "done");

end Lesson_5A;
