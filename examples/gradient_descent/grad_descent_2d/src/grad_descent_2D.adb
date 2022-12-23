
--  with Ada.Containers.Vectors;
with Ada.Text_IO; use Ada.Text_IO;

--  with Maths;

--  with Basic_Printing; use Basic_Printing;
with ML_Arrays_And_Matrices; use ML_Arrays_And_Matrices;
with Python; use Python;

--  with ML; use ML;
--  with Support_5A; use Support_5A;

procedure Grad_Descent_2D is

   --     package Float2_Package is new
   --       Ada.Containers.Vectors (Positive, Float2_Array);
   --     subtype Float2_List is Float2_Package.Vector;

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
   Data        : Real_Float_Matrix (1 .. Num_Samples, 1 .. 2);
   Labels      : Real_Float_Vector (1 .. Num_Samples);
begin
   declare
      --        Labels                : Integer_Array (1 .. Num_Samples);
      Py_Module             : Module;
      First                 : constant Float := -1.0;
      Last                  : constant Float := 3.0;
      Step                  : constant Float := (Last - First) / Float (Num_Samples);
      Val                   : Float;
      --        Weights               : Real_Float_Vector :=
      --                                  (0.786,  0.175, -0.558, -0.437);
   begin
      for row in 1 .. Num_Samples loop
         Val := First + Float ((row - 1)) * Step;
         for col in 1 .. 2 loop
            Data (row, col) := Val;
         end loop;
         Labels (row) := F (Data (row, 1), Data (row, 2));
      end loop;

      Python.Initialize;
      Py_Module := Import_File ("grad_descent_2d");
      Python.Call (Py_Module, "show_contours", Data, Labels);
      New_Line;
      Python.Finalize;
      Put_Line (Project_Name & "done");

   end;

end Grad_Descent_2D;
