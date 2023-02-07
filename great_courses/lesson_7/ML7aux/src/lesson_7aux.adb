
with Ada.Text_IO; use Ada.Text_IO;

with Maths;

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
   Population   : Real_Float_Vector (1 .. 10);
   Children     : Real_Float_Vector (Population'Range);
   Generation   : Real_Float_Vector (1 .. 20);
   Values       : Real_Float_Vector (Population'Range)  :=
                    (others => 0.0);
   Gen_Values   : Real_Float_Vector (Generation'Range);
   Xs_Ys        : XY_Data;
   Traject      : XY_Data;
   Threshold    : Float;
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

   Threshold := Median (Values);

   Put_Line ("Minimum value: " & Float'Image (Min (Values)));
   Put_Line ("Threshold: " & Float'Image (Threshold));

   Python.Call (Classifier, "plot_values", Angles, Landing,
                Population, Values);

   for index in Population'Range loop
      if  Values (index) < Threshold then
         Population (index) :=
           Float (Maths.Random_Integer (0, 3600)) / 10.0;
         Values (index) := Shoot (Population (index));
         Children (index) := Population (index) + 5.0 * Maths.Random_Float;
         Generation (index) := Population (index);
         Generation (index + Population'Last) := Children (index);
      else
         Population (index) := 0.0;
      end if;
   end loop;

   Python.Call (Classifier, "plot_values", Angles, Landing,
                Population, Values);

   for index in Generation'Range loop
      Gen_Values (index) := Shoot (Generation (index));
   end loop;
   Put_Line ("Minimum value: " & Float'Image (Min (Gen_Values)));
   Python.Call (Classifier, "plot_values", Angles, Landing,
                Generation, Gen_Values);

   Traject := Trajectory (180.0);
   Python.Call (Classifier, "plot_xy", Traject.Xs, Traject.Ys);
   Traject := Trajectory (60.0);
   Python.Call (Classifier, "plot_xy", Traject.Xs, Traject.Ys);
   Traject := Trajectory (280.0);
   Python.Call (Classifier, "plot_xy", Traject.Xs, Traject.Ys);

   Python.Call (Classifier, "show_plot", 20, 0);
   New_Line;
   Traject := Trajectory (42.0);
   Python.Call (Classifier, "plot_xy", Traject.Xs, Traject.Ys);
   Traject := Trajectory (90.0);
   Python.Call (Classifier, "plot_xy", Traject.Xs, Traject.Ys);
   Traject := Trajectory (138.0);
   Python.Call (Classifier, "plot_xy", Traject.Xs, Traject.Ys);
   Traject := Trajectory (200.0);
   Python.Call (Classifier, "plot_xy", Traject.Xs, Traject.Ys);
   Traject := Trajectory (270.0);
   Python.Call (Classifier, "plot_xy", Traject.Xs, Traject.Ys);
   Traject := Trajectory (340.0);
   Python.Call (Classifier, "plot_xy", Traject.Xs, Traject.Ys);

   Python.Call (Classifier, "show_plot", 20, 0);

   Python.Finalize;

   Put_Line (Project_Name & "finished.");
   New_Line;

end Lesson_7Aux;
