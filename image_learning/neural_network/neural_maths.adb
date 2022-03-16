
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

    --  --------------------------------------------------------------------------

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
    --  Hurwitz zeta function
    function Zeta (S : Natural; A : Float) return Float is
        Result : Float := 0.0;
    begin
        for index in 0 .. 100 loop
            Result := Result + 1.0 / ((Float (index) + A) ** S);
        end loop;

        return Result ;

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
        while Continue and then index <= 100 loop
            Index := Index + 1;
            Coeff := -Coeff * Value;
            Term := Coeff * Zeta (index, Root);
            Result := Result + Term;
            Continue := abs (Term) >= Tol * abs (Result);
        end loop;

        return Result ;

    end Zeta_Series;

    --  -------------------------------------------------------------------------

end Neural_Maths;
