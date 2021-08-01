
with Ada.Numerics.Generic_Elementary_Functions;

package Maths is

   type Degree is new Float;
   type Radian is new Float;

   package Float_Math_Functions is new
     Ada.Numerics.Generic_Elementary_Functions (Float);

   function Cube_Root (Value : Float) return Float;
   function Degrees (Angle : Radian) return Degree;
   function Radians (Angle : Degree) return Radian;
   function Random_Float return Float;
end Maths;
