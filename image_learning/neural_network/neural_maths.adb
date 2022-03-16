
with Ada.Assertions; use Ada.Assertions;

with Maths;

with Weights;

package body Neural_Maths is

   --  Based on github.com/scipy/scipy/blob/main/scipy/special/_digamma.pxd

   Small_Abs    : constant Integer := 16;
   Small_Imag   : constant Integer := 6;
   Tol          : constant Float := 2.220446092504131 * 10.0 ** (-16);
   --  The following constants were computed with mpmath
   --  Location and value of the positive root
   Pos_Root     : constant Float := 1.4616321449683623;
   Pos_Root_Val : constant Float := -9.2412655217294275 * 10.0 ** (-17);
   --  Location and value of the negative root
   Neg_Root     : constant Float := -0.504083008264455409;
   Neg_Root_Val : constant Float := 7.2897639029768949 * 10.0 ** (-17);

   function Zeta_Series (Z, Root, Root_Val : Float) return Float;

   --  -------------------------------------------------------------------------

   function Digamma (Z : Float) return Float is
      Result : Float;
   begin
      if abs (Z - Neg_Root) < 0.3 then
         null;
      else
         null;
      end if;

      return Result;

   end Digamma;

   --  --------------------------------------------------------------------------
   --  Based on github.com/scipy/scipy/blob/main/scipy/special/_logsumexp.py
   --  logsumexp (a, axis=None, b=None, keepdims=False, return_sign=False)
   function Log_Sum_Exponent (Log_Prob : Float_List) return Float is
      use Maths.Float_Math_Functions;
      Log_Prob_Max : constant Float := Weights.Max (Log_Prob);
      Diff         : Float;
      Exp_Delta    : Float_List;
      Sum          : Float := 0.0;
   begin
      for index in Log_Prob.First_Index .. Log_Prob.Last_Index loop
         Diff := Log_Prob.Element (index) - Log_Prob_Max;
         if Diff = 0.0 then
            Exp_Delta.Append (1.0);
         else
            Exp_Delta.Append (Exp (Diff));
         end if;
      end loop;

      for index in Exp_Delta.First_Index .. Exp_Delta.Last_Index loop
         Sum := Sum + Exp_Delta.Element (index);
      end loop;

      return Log (Sum);

   end Log_Sum_Exponent;

   --  -------------------------------------------------------------------------

   function Mean (A : Float_List) return Float is
      Result : Float := 0.0;
   begin
      for index in A.First_Index .. A.Last_Index loop
         Result := Result + A (index);
      end loop;
      return Result / Float (A.Length);

   end Mean;

   --  -------------------------------------------------------------------------
   --  Based on https://people.sc.fsu.edu/~jburkardt/f77_src/asa103/asa103.f
   --  Applied Statistics, Volume 25, Number 3, 1976, pages 315-317
   function Psi (X : Float) return Float is
      use Maths.Float_Math_Functions;
      Routine_Name : constant String := "Neural_Maths.Psi ";
      Euler_Mascheroni : constant Float := 0.5772156649015328606;
      R                : Float;
      X2               : Float := X;
      Digamma          : Float := 0.0;
   begin
      Assert (X > 0.0, Routine_Name & "X," & Float'Image (X) &
                " should be greater than zero.");

      if X <= 0.00001 then
         Digamma := -Euler_Mascheroni - 1.0 / X2;
      else
         --  Reduce to Digamma (X + N).
         while X2 < 8.5 loop
            Digamma := Digamma - 1.0 / X2;
            X2 := X2 + 1.0;
         end loop;

         --  Use Stirling's (actually de Moivre's) expansion.
         R := 1.0 / X2;
         Digamma := Digamma + Log (X2) - 0.5 * R;
         R := R ** 2;
         Digamma := Digamma - R / 12.0  - R / 120.0 - R / 252.0;
      end if;

      return Digamma;

   end Psi;

   --  ------------------------------------------------------------------------
  --  Riemann zeta function of two arguments
   function Zeta (X : Natural; Q : Float) return Float is
      Routine_Name : constant String := "Neural_Maths.Zeta ";
      k            : Natural := 0;
      Term         : Float;
      Continue     : Boolean := True;
      Result       : Float := 0.0;
   begin
      Assert (X > 1, Routine_Name & "X," & Integer'Image (X) &
                " should be greater than 1.");
      Assert (Q > 0.0, Routine_Name & "Q," & Float'Image (Q) &
                " should be greater than zero.");

      while Continue and then k <= 100 loop
         Term := 1.0 / ((Float (k) + Q) ** X);
         Result := Result + Term;
         k := k + 1;
         Continue := abs (Term) > Tol * abs (Result);
      end loop;

      return Result;

   end Zeta;

   --  ------------------------------------------------------------------------

   function Zeta_Series (Z, Root, Root_Val : Float) return Float is
      Value    : constant Float := Z - Root;
      Coeff    : Float := -1.0;
      Term     : Float;
      Index    : Natural := 0;
      Continue : Boolean := True;
      Result   : Float := Root_Val;
   begin
      while Continue and then Index <= 100 loop
         Index := Index + 1;
         Coeff := -Coeff * Value;
         Term := Coeff * Zeta (index, Root);
         Result := Result + Term;
         Continue := abs (Term) > Tol * abs (Result);
      end loop;

      return Result ;

   end Zeta_Series;

   --  -------------------------------------------------------------------------

end Neural_Maths;
