
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

    --      function Poisson (Mean : Float := 0.0) return Integer is
    --          use Ada.Numerics.Float_Random;
    --          use Float_Math_Functions;
    --          P      : Float := Exp (-Mean);
    --          F      : Float := P;
    --          U      : Float := Random (Float_Gen);
    --          Result : Integer := 0;
    --      begin
    --          while U > F loop
    --              Result := Result + 1;
    --              P := (Mean / Float (Result)) * P;
    --              F :=  F + P;
    --          end loop;
    --
    --          return Result;
    --
    --      end Poisson;
    --
    --      --  -----------------------------------------------------------------------

    function Poisson (Lambda : Float) return Integer is
        use Ada.Numerics.Float_Random;
        use Float_Math_Functions;
        Sum_T  : Float := 0.0;
        E      : Float;
        Result : Integer := -1;
    begin
        while Sum_T < 1.0 loop
            E := -(1.0 / Lambda) * Log (Random (Float_Gen));
            Sum_T := Sum_T + E;
            Result := Result + 1;
        end loop;

        return Result;

    end Poisson;

    --  -----------------------------------------------------------------------
    --  Based on https://github.com/hpaulkeeler/posts/blob/master/
    --  PoissonDirect/PoissonDirectSingle.c
    function Poisson_Single (Lambda : Float) return Integer is
        use Ada.Numerics.Float_Random;
        use  Float_Math_Functions;
        --  loop termination constant
        Exp_Lambda      : constant Float := Exp (-Lambda);
        Rand_Uniform    : Uniformly_Distributed;        --  range: 0.0 .. 1.0
        Product_Uniform : Float := 1.0; --  Product of uniform variables
        Rand_Poisson    : Integer := -1;
    begin
        while Product_Uniform > Exp_Lambda  loop
            Rand_Uniform := Random (Float_Gen);
            Product_Uniform := Product_Uniform * Float (Rand_Uniform);
            Rand_Poisson := Rand_Poisson + 1;
        end loop ;

        return Rand_Poisson;

    end Poisson_Single;

    --  ------------------------------------------------------------------------

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

    function Random_Integer (First, Last : Integer) return Integer is
      subtype R_Range is Integer range First .. Last;
      package Random_Integer_Package is new
        Ada.Numerics.Discrete_Random (R_Range);
      use Random_Integer_Package;
      Gen : Generator;
    begin
      return Random (Gen);

    end Random_Integer;

    --  ------------------------------------------------------------------------

end Maths;
