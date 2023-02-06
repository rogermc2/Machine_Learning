
with Ada.Text_IO; use Ada.Text_IO;

--  with Basic_Printing; use Basic_Printing;
with ML_Arrays_And_Matrices; use ML_Arrays_And_Matrices;
with Python;
--  with Python_CLF;
--  with Python_API;

with Support_7Aux; use Support_7Aux;

procedure Lesson_7Aux is

   Project_Name : constant String := "Lesson 7Aux ";
   Steps        : constant Positive := 3600;
   Angles       : Real_Float_Vector (1 .. Steps);
   Landing      : Real_Float_Vector (Angles'Range);
   Classifier   : Python.Module;
begin
   for index in Angles'Range loop
      Angles (index) := 360.0 * Float (index) / Float (Steps);
   end loop;

   for index in Landing'Range loop
      Landing (index) := Shoot (Angles (index));
   end loop;

   Python.Initialize;
   Classifier := Python.Import_File ("lesson_7aux");

   Python.Call (Classifier, "plot_data", Angles, Landing);

   Python.Finalize;

   Put_Line (Project_Name & "finished.");
   New_Line;

end Lesson_7Aux;
