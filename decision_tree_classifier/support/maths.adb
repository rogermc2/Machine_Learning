
with Ada.Numerics;
with Ada.Numerics.Float_Random;

package body Maths is
   Radians_Per_Degree : constant Radian := Ada.Numerics.Pi / 180.0;
   Degrees_Per_Radian : constant Degree := 180.0 / Ada.Numerics.Pi;

   Gen     : Ada.Numerics.Float_Random.Generator;

   --  ------------------------------------------------------------------------

   function Cube_Root (Value : Float) return Float is
   begin
      return Float_Math_Functions.Exp (Float_Math_Functions.Log (Value) / 3.0);
   end Cube_Root;

   --  ------------------------------------------------------------------------

   function Degrees (Angle : Radian) return Degree is
   begin
      return Degree (Angle) * Degrees_Per_Radian;
   end Degrees;

   --  -----------------------------------------------------------------------
  
   function Radians (Angle : Degree) return Radian is
   begin
      return Radian (Angle) * Radians_Per_Degree;
   end Radians;

   --  ------------------------------------------------------------------------

   function Random_Float return Float is
      use Ada.Numerics.Float_Random;
   begin
      return 2.0 * Float (Random (Gen)) - 1.0;
   end Random_Float;

   --  ------------------------------------------------------------------------

   function Random_Integer return Integer is
   begin
      return Integer (Random_Float);
   end Random_Integer;

   --  ------------------------------------------------------------------------

end Maths;
