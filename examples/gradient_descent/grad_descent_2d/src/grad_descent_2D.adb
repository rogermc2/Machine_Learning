
--  with Ada.Containers.Vectors;
with Ada.Text_IO; use Ada.Text_IO;

--  with Maths;

--  with Basic_Printing; use Basic_Printing;
with ML_Arrays_And_Matrices; use ML_Arrays_And_Matrices;
with Python; use Python;

--  with ML; use ML;
--  with Support_5A; use Support_5A;

procedure Grad_Descent_2D is

   type Float2_Array is array (Integer range 1 .. 2) of Float;

--     package Float2_Package is new
--       Ada.Containers.Vectors (Positive, Float2_Array);
--     subtype Float2_List is Float2_Package.Vector;

   Project_Name    : constant String := "Grad_Descent_2D ";

   function F (xy : Float2_Array) return Float is
   begin
      return 6.0 * xy (1)**2 + 9.0 * xy (2)**2 - 12.0 * xy (1) - 14.0 * xy (2)
        + 3.0;
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
   Data        : array (1 .. Num_Samples) of Float2_Array;
   z           : Real_Float_Vector (1 .. Num_Samples);
begin
   declare
      Labels                : Integer_Array (1 .. Num_Samples);
      Py_Module             : Module;
      First     : constant Float := -1.0;
      Last      : constant Float := 3.0;
      Step      : constant Float := (Last - First) / Float (Num_Samples);
      Val       : Float;
      Weights               : Real_Float_Vector :=
                                (0.786,  0.175, -0.558, -0.437);
   begin
      for index in 1 .. Num_Samples loop
         Val := First + Float ((index - 1)) * Step;
         Data (index) := (Val, Val);
      end loop;

      Python.Initialize;
      Py_Module := Import_File ("grad_descent_2d");

      New_Line;
      Python.Finalize;
      Put_Line (Project_Name & "done");

   end;

end Grad_Descent_2D;
