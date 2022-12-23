
with Ada.Text_IO; use Ada.Text_IO;

with ML_Arrays_And_Matrices; use ML_Arrays_And_Matrices;
with Python; use Python;

procedure Grad_Descent_2D is

   Project_Name    : constant String := "Grad_Descent_2D ";

   function F (x, y : Float) return Float is
   begin
      return 6.0 * x**2 + 9.0 * y**2 - 12.0 * x - 14.0 * y + 3.0;
   end F;

   function dx (x : Float) return Float is
   begin
      return 12.0 * x - 12.0;
   end dx;

   function dy (y : Float) return Float is
   begin
      return 18.0 * y - 14.0;
   end dy;

   Num_Samples : constant Positive := 100;
   First       : constant Float := -1.0;
   Last        : constant Float := 3.0;
   Step        : constant Float :=
                   (Last - First) / Float (Num_Samples);
   X           : Real_Float_Vector (1 .. Num_Samples);
   Y           : Real_Float_Vector (1 .. Num_Samples);
   Z           : Real_Float_Matrix (1 .. Num_Samples, 1 .. Num_Samples);
   Val         : Float;
   X_Old       : Float := -0.5;
   Y_Old       : Float := 5.9;
   Py_Module   : Module;
begin
   for index in 1 .. Num_Samples loop
      Val := First + Float ((index - 1)) * Step;
      X (index) := Val;
      Y (index) := Val;
   end loop;

   for row in 1 .. Num_Samples loop
      for col in 1 .. Num_Samples loop
--           Z (row, col) := (6.0 * Float (row))**2 + (9.0 * Float (col))**2;
         Z (row, col) := F (X (row), Y (col));
      end loop;
   end loop;

   Python.Initialize;
   Py_Module := Import_File ("grad_descent_2d");
   Python.Call (Py_Module, "show_contours", X, Y, Z);
   New_Line;
   Python.Finalize;
   Put_Line (Project_Name & "done");

end Grad_Descent_2D;
