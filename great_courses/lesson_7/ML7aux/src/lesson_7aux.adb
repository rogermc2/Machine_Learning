
with Ada.Text_IO; use Ada.Text_IO;

with Maths;

with Basic_Printing; use Basic_Printing;
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
   Population   : Real_Float_Vector (1 .. 10);
   Values       : Real_Float_Vector (Population'Range) :=
                    (others => 0.0);
   Xs_Ys        : XY_Data;
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

--     Python.Call (Classifier, "plot_data", Angles, Landing);

   Xs_Ys := Show (360.0);
--     Python.Call (Classifier, "plot_launcher", Xs_Ys.Xs, Xs_Ys.Ys);

   for index in Population'Range loop
      Population (index) :=
        Float (Maths.Random_Integer (0, 3600)) / 10.0;
      Values (index) := Shoot (Population (index));
   end loop;
   Print_Float_Vector ("Population", Population);

   Python.Call (Classifier, "plot_values", Angles, Landing,
               Population, Values);

   Python.Finalize;

   Put_Line (Project_Name & "finished.");
   New_Line;

end Lesson_7Aux;
