
with Ada.Numerics;
with Ada.Numerics.Discrete_Random;
with Ada.Numerics.Float_Random;

package body Maths is
   package Random_Integer_Package is new 
     Ada.Numerics.Discrete_Random (Random_Integer_Range);
   
   Radians_Per_Degree : constant Radian := Ada.Numerics.Pi / 180.0;
   Degrees_Per_Radian : constant Degree := 180.0 / Ada.Numerics.Pi;

   Float_Gen           : Ada.Numerics.Float_Random.Generator;
   Integer_Gen         : Random_Integer_Package.Generator;

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
   --  Random_Float generates a random number in the range  -1.0 .. 1.0
   function Random_Float return Float is
      use Ada.Numerics.Float_Random;
      --  Random range 0.0 .. 1.0
   begin
      return 2.0 * Float (Random (Float_Gen)) - 1.0;
   end Random_Float;

   --  ------------------------------------------------------------------------

   function Random_Integer return Integer is
      use Random_Integer_Package;
   begin
      return Integer (Random (Integer_Gen));
   end Random_Integer;

   --  ------------------------------------------------------------------------

end Maths;
