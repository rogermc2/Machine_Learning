
with Ada.Assertions; use Ada.Assertions;
with Ada.Numerics;

with Maths;

package body Neural_Maths is

    --     Small_Abs    : constant Integer := 16;
    --     Small_Imag   : constant Integer := 6;
    Tol          : constant Float := 2.220446092504131 * 10.0 ** (-16);
    --  The following constants were computed with mpmath
    --  Location and value of the positive root
    --     Pos_Root     : constant Float := 1.4616321449683623;
    --     Pos_Root_Val : constant Float := -9.2412655217294275 * 10.0 ** (-17);
    --  Location and value of the negative root
    Neg_Root     : constant Float := -0.504083008264455409;
    Neg_Root_Val : constant Float := 7.2897639029768949 * 10.0 ** (-17);

    function Psi_Cephes (X : Float) return Float;
    function Zeta_Series (Z, Root, Root_Val : Float) return Float;

    --  -------------------------------------------------------------------------
    --  Based on github.com/scipy/scipy/blob/main/scipy/special/_digamma.pxd
    function Digamma (Z : Float) return Float is
        Result : Float;
    begin
        if abs (Z - Neg_Root) < 0.3 then
            Result := Zeta_Series (Z, Neg_Root, Neg_Root_Val);
        else
            Result := Psi_Cephes (Z);
        end if;

        return Result;

    end Digamma;

    --  --------------------------------------------------------------------------

    function Mean (A : Integer_Matrix) return Float is
        Length : constant Float := Float (A'Length * A'Length (2));
        Sum    : Integer := 0;
    begin
        for row in A'First .. A'Last loop
            for col in A'First (2) .. A'Last (2) loop
                Sum := Sum + A (row, col);
            end loop;
        end loop;

        return Float (Sum) / Length;

    end Mean;

    --  ------------------------------------------------------------------------

    function Mean (A : Real_Float_Matrix) return Real_Float_Vector is
        Length : constant Positive := A'Length (2);
        Sum    : Float;
        Result : Real_Float_Vector (1 .. Length);
    begin
        for col in A'Range (2) loop
            Sum := 0.0;
            for row in A'Range loop
                Sum := Sum + A (row, col);
            end loop;
            Result (col) := Sum / Float (Length);
        end loop;

        return Result;

    end Mean;

    --  ------------------------------------------------------------------------

    function Mean (A : Real_Float_Matrix) return Float is
        Length  : constant Float := Float (A'Length * A'Length (2));
        Sum     : Float := 0.0;
    begin
        for row in A'First .. A'Last loop
            for col in A'First (2) .. A'Last (2) loop
                Sum := Sum + A (row, col);
            end loop;
        end loop;

        return Sum / Length;

    end Mean;

    --  ------------------------------------------------------------------------
    --  Based on github.com/scipy/scipy/blob/main/scipy/special/cephes/polevl.h
    --  Pol_Eval evaluates polynomial of degree N:
    --  y  =  C0  + C1 x + C2 x^2  +...+ CN x^N
    --  Coefficients are stored in reverse order:
    --  coef [0] = CN  , ..., coef[N] = C0
    function Pol_Eval (X : Float; Coeff : Real_Float_Vector; N : Positive)
                   return Float is
        Result : Float := 0.0;
    begin
        for index in Coeff'First .. Coeff'Last loop
            Result := Coeff (index) * Result;
        end loop;

        for index in reverse 1 .. N loop
            Result := Result * X + Coeff (index) * Result;
        end loop;

        return Result;

    end Pol_Eval;

    --  ------------------------------------------------------------------------
    --  Based on github.com/poliastro/cephes/blob/master/src/psi.c
    --  Psi (X) function
    --  Wikipedia digamma function:
    --  Psi (X) = d ln (gamma (X)) / dx ~ ln (X) - 1 / 2x
    function Psi_Cephes (X : Float) return Float is
        use Ada.Numerics;
        use Maths.Float_Math_Functions;
        Routine_Name : constant String := "Neural_Maths.Psi_Cephes ";
        Euler        : constant Float := 0.57721566490153286061;
        A            : constant Real_Float_Vector (0 .. 6) :=
                         ( 0.00833333333333333333333,
                           -0.00210927960927960927961,
                           0.000757575757575757575758,
                           -0.000416666666666666666667,
                           0.000396825396825396825397,
                           -0.000833333333333333333333,
                           0.00833333333333333333333);
        Negative     : constant Boolean := X <= 0.0;
        X2           : Float := X;
        P            : Float := Float'Floor (X);
        NZ           : Float := 0.0;
        N            : Positive;
        W            : Float;
        S            : Float;
        Z            : Float;
        Result       : Float := 0.0;
    begin
        if Negative then
            Assert (P /= X, Routine_Name & "X should be > Floor (X).");
            --  Remove the zeros of Tan (PI x) by subtracting the nearest
            --  integer from x
            NZ := X - P;
            if NZ = 0.5 then
                NZ := 0.0;
            elsif NZ > 0.5 then
                P := P + 1.0;
                NZ := X - P;
            elsif NZ < 0.5 then
                NZ := Pi / Tan (Pi / NZ);
            end if;
            X2 := 1.0 - X2;
        end if;

        --  check for positive integer up to 10
        if X2 <= 10.0 and then X2 = Float'Floor (X2) then
            N := Positive (X2);
            for index in 1 .. N loop
                Result := Result + 1.0 / Float (index);
            end loop;
            Result := Result - Euler;
        else
            S := X2;
            W := 0.0;
            while S < 10.0 loop
                W := W + 1.0 / S;
                S := S + 1.0;
            end loop;

            if S < 10.0 ** 17 then
                Z := 1.0 / (S ** 2);
                Result := Z * Pol_Eval (Z, A, 6);
            else
                Result := 0.0;
            end if;

        end if;

        return Result;

    end Psi_Cephes;

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
