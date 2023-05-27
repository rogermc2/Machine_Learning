
--  with Ada.Assertions; use Ada.Assertions;
with Ada.Exceptions; use Ada.Exceptions;
with Ada.Text_IO; use Ada.Text_IO;

--  with Basic_Printing; use  Basic_Printing;
with ML_Arrays_And_Matrices; use ML_Arrays_And_Matrices;
with Python;

with Support_15A; use Support_15A;

procedure Lesson_15A is
   Program_Name : constant String := "Lesson 15A ";
   Num_Samples  : constant  Positive := 120;
   Test_Size    : constant  Positive := Positive (0.1 * Float (Num_Samples));
   Train_Size   : constant  Positive := Positive (0.2 * Float (Num_Samples));
   Train_X      :  Image_Vector (1 .. 2 * Train_Size);
   Train_Y      :  Integer_Array (1 .. 2 * Train_Size);
   Test_X       :  Image_Vector (1 .. 2 * Test_Size);
   Test_Y       :  Integer_Array (1 .. 2 * Test_Size);
   Classifier   : Python.Module;
   Test         : Image_Array;
begin
   Put_Line (Program_Name);
   Python.Initialize;

   Classifier := Python.Import_File ("lesson_15a");
   Build_Data (Num_Samples, Train_Size, Test_Size, Train_X, Test_X, Train_Y, Test_Y);
   Test := Train_X (1);
   Python.Call (Classifier, "show_bitmap", Test);

   New_Line;

   Python.Close_Module (Classifier);
   Python.Finalize;

   Put_Line ("----------------------------------------------");

   Put_Line (Program_Name & "finished.");
   New_Line;

exception
   when Error: Constraint_Error => Put_Line (Program_Name &
                                               "Constraint_Error");
      Put_Line (Exception_Information(Error));
   when Error: others => Put_Line (Program_Name & "exception");
      Put_Line (Exception_Information(Error));

end Lesson_15A;
