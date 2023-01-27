
with Ada.Text_IO; use Ada.Text_IO;

--  with Basic_Printing; use Basic_Printing;
with ML_Arrays_And_Matrices; use ML_Arrays_And_Matrices;
with Python; use Python;

--  with Maths;

with ML; use ML;
with Support_6A; use Support_6A;

procedure Lesson_6A is

   type Integer3_Array is array (Integer range 1 .. 3) of Integer;

   Project_Name           : constant String := "Lesson_6A ";
   Green_File_Name        : constant String := "../greenML.png";
   Forest_File_Name       : constant String := "../forest.jpg";
   Py_Module              : Module;
   Weights                : Real_Float_Vector (1 .. 4);
begin

   Python.Initialize;
   Py_Module := Import_File ("lesson_6a");
   --     Python.Call (Py_Module, "show_bitmap", Image_Data);

   --  Train the model.

   Python.Finalize;

   Put_Line (Project_Name & "finished.");

end Lesson_6A;
