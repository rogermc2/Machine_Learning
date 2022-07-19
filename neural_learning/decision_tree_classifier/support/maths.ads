
with Ada.Numerics.Generic_Elementary_Functions;

package Maths is

   type Degree is new Float;
   type Radian is new Float;
   type Random_Integer_Range is range -Integer'Last + 1 .. Integer'Last - 1;

   package Float_Math_Functions is new
     Ada.Numerics.Generic_Elementary_Functions (Float);
   package Long_Float_Math_Functions is new
     Ada.Numerics.Generic_Elementary_Functions (Long_Float);

   function Cube_Root (Value : Float) return Float;
   function Degrees (Angle : Radian) return Degree;
   function Poisson (Lambda : Float) return Integer;
   function Poisson_Single (Lambda : Float) return Integer;
   function Radians (Angle : Degree) return Radian;
   function Random_Float return Float;
   function Random_Integer return Integer;
end Maths;
