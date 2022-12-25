
with Ada.Text_IO; use Ada.Text_IO;

with Maths;

with ML_Arrays_And_Matrices; use ML_Arrays_And_Matrices;
with Python; use Python;

procedure Grad_Descent_Multi_Min is
   use Maths.Float_Math_Functions;
   Project_Name    : constant String := "Grad_Descent_Multi_Min ";

   function F (x, y : Float) return Float is
   begin
      return -2.0 * Exp (-0.5 * ((x + 1.0) ** 2 + (y - 1.0) ** 2)) -
        Exp (-0.5 * ((x - 1.0) ** 2 + (y + 1.0) ** 2));
   end F;

   function dx (x, y : Float) return Float is
   begin
      return 2.0 * (x + 1.0) * Exp (-0.5 * ((x + 1.0) ** 2 + (y - 1.0) ** 2)) +
        (x - 1.0) * Exp (-0.5 * ((x - 1.0) ** 2 + (y + 1.0) ** 2));
   end dx;

   function dy (x, y : Float) return Float is
   begin
      return 2.0 * (y - 1.0) * Exp (-0.5 * ((x + 1.0) ** 2 + (y - 1.0) ** 2)) +
        (y + 1.0) * Exp (-0.5 * ((x - 1.0) ** 2 + (y + 1.0) ** 2));
   end dy;

   procedure Plot_Points (Py_Module : Module; X_Start, Y_Start,
                          Alpha     : Float; Marker : String;
                          Num_Points : Positive) is
      X     : Float := X_Start;
      Y     : Float := Y_start;
      X_Old : Float := X;
      Y_Old : Float := Y;
   begin
      for index in 1 .. Num_Points loop
         Python.Call (Py_Module, "plot_data", X_Old, X, Y_Old, Y, Marker);
         X_Old := X;
         Y_Old := Y;
         X := X - Alpha * dx (X, Y);
         Y := Y - Alpha * dy (X, Y);
      end loop;

   end Plot_Points;

   Num_Samples : constant Positive := 100;
   First       : constant Float := -2.0;
   Last        : constant Float := 2.0;
   Step        : constant Float :=
                   (Last - First) / Float (Num_Samples);
   X_Data      : Real_Float_Vector (1 .. Num_Samples);
   Y_Data      : Real_Float_Vector (1 .. Num_Samples);
   Z           : Real_Float_Matrix (1 .. Num_Samples, 1 .. Num_Samples);
   Val         : Float;
   Py_Module   : Module;
begin
   for index in 1 .. Num_Samples loop
      Val := First + Float ((index - 1)) * Step;
      X_Data (index) := Val;
      Y_Data (index) := Val;
   end loop;

   for row in 1 .. Num_Samples loop
      for col in 1 .. Num_Samples loop
         Z (row, col) := F (X_Data (row), Y_Data (col));
      end loop;
   end loop;

   Python.Initialize;
   Py_Module := Import_File ("grad_descent_multi_min");
   Python.Call (Py_Module, "show_contours", X_Data, Y_Data, Z);

   Plot_Points (Py_Module, -1.5, 1.2, 0.4, "o", 9);
   Plot_Points (Py_Module, 1.5, -1.8, 0.4, "s", 9);
   Plot_Points (Py_Module, 0.0, 0.0, 0.4, "+", 20);
   Plot_Points (Py_Module, 0.7, -0.2, 0.4, "^", 20);
   Plot_Points (Py_Module, 1.5, 1.5, 0.4, "*", 30);

   Python.Call (Py_Module, "show");
   New_Line;
   Python.Finalize;
   Put_Line (Project_Name & "done");

end Grad_Descent_Multi_Min;
