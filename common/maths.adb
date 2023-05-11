
with Ada.Numerics;
with Ada.Numerics.Discrete_Random;
with Ada.Numerics.Float_Random;

package body Maths is
   package Random_Integer_Package is new
     Ada.Numerics.Discrete_Random (Random_Integer_Range);

--     type Func_Type is access function (X : Float) return Float;

   Radians_Per_Degree : constant Radian := Ada.Numerics.Pi / 180.0;
   Degrees_Per_Radian : constant Degree := 180.0 / Ada.Numerics.Pi;

   Float_Gen           : Ada.Numerics.Float_Random.Generator;
   Integer_Gen         : Random_Integer_Package.Generator;

--     function Integrate (Func : Func_Type; A, B : Float; N : Positive)
--                         return Float;

   --  ------------------------------------------------------------------------
   --  Binomial coeffient defined by G. Woan equation (2.122)
   --  Factorial(<1) returns 1.
   function Binomial_Coefficient (N, B : Integer) return Float is
   begin

      return Float (Factorial (N)) / Float (Factorial (N) * Factorial (n - B));

   end Binomial_Coefficient;

   --  ------------------------------------------------------------------------
   --  Binomial_Distribution returns the probability of X successes occurring
   --  in N trials when the probability of one success is P.
   --  Binomial PMF (x) for specified N and P
   --  N = the number of trials (or the number being sampled)
   --  × = the number of successes desired
   --  P = the probability of getting a success in one trial
   --  Q = 1 - P = the probability of getting a failure in one trial
   function Binomial_Distribution (N : Integer; P : Float) return Float is
      Q : constant Float := 1.0 - P;
      X : constant Positive := Random_Integer (0, N);
   begin

      return Binomial_Coefficient (N, X) * P ** X * Q ** (N - X);

   end Binomial_Distribution;

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

   Function Factorial (N : Integer) return Integer is
      Result : Integer := 1;
   begin
      if n > 1 then
         Result := n * Factorial(n - 1);
      end if;
      return result;

   end Factorial;

   -- ------------------------------------------------------------

   --        function Trapezium (A, B : Float; N : Positive) return Scalar is
   --           Sum : Float := F(A) + F(B);
   --           X   : Float := 1.0;
   --        begin
   --           while X <= Float (N) - 1.0 loop
   --              Sum := Sum + 2.0 * F (A + X * (B - A) / Float (N));
   --              X := X + 1.0;
   --           end loop;
   --        return (B - A) / (2.0 * Float (N)) * Sum;
   --
   --        end Trapezium;

   -- --------------------------------------------------------------------------
   --  Integrate using Simpson's rule
--     function Integrate (Func : Func_Type; A, B : Float; N : Positive)
--                         return Float is
--        H     : constant Float := (B - A) / Float (N);
--        Sum_U : Float := 0.0;
--        Sum_E : Float := 0.0;
--     begin
--        for index in 1 .. N - 1 loop
--           if index mod 2 /= 0 then
--              Sum_U := Sum_U + Func (A + H * Float (index));
--           else
--              Sum_E := Sum_E + Func (A + H * Float (index));
--           end if;
--        end loop;
--
--        return (H / 3.0) * (Func (A) + Func (B) + 4.0 * Sum_U + 2.0 * Sum_E);
--
--     end Integrate;

   -- ------------------------------------------------------------

   function Normal_Distribution (Mu : Float := 0.0; Sigma : Float := 1.0)
                                 return Float is
      use Ada.Numerics;
      use Float_Random;
      use Float_Math_Functions;
   begin
      return Mu + Sigma * Sqrt (-2.0 * Log (Random (Float_Gen), 10.0)) *
        Cos (2.0 * Pi * Random (Float_Gen));

   end Normal_Distribution;

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
   --      --  -----------------------------------------------------------------

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

   --  ------------------------------------------------------------------------   --  Based on Numerical Recipes, Figure 7.3.1
   --  Based on Numerical Recipes, Figure 7.3.1
   function Random_Binomial (N : Integer; P : Float) return Integer is
      Ran    : constant Float := Float (Random_Integer (0, 1));
      Index  : Integer := 0;
      Found  : Boolean := False;
      Result : Integer := 0;
   begin
      while not Found and then Index <= N  loop
         Found := Binomial_Distribution (Index, P) >= Ran;
         if Found then
            Result := Index;
         end if;
         Index := Index + 1;
      end loop;

      return Result;

   end Random_Binomial;

   --  ------------------------------------------------------------------------
   --  Random_Float generates a random number in the range  -1.0 .. 1.0
   function Random_Float return Float is
      use Ada.Numerics.Float_Random;
      --  Random range 0.0 .. 1.0
   begin
      Reset (Float_Gen);
      return 2.0 * Float (Random (Float_Gen)) - 1.0;
   end Random_Float;

   --  ------------------------------------------------------------------------

   function Random_Integer return Integer is
      use Random_Integer_Package;
   begin
      Reset (Integer_Gen);
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
      Reset (Gen);
      return Random (Gen);

   end Random_Integer;

   --  ------------------------------------------------------------------------

   function Round (Num : Float; Places : Natural) return Float is
   begin
      return Float'Rounding (Num * 10.0 ** Places) / 10.0 ** (-Places);

   end Round;

   --  ------------------------------------------------------------------------

end Maths;
