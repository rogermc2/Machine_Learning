--  Based on scipy/optimize/lbfgsb_py.py

with Ada.Assertions; use Ada.Assertions;
with Ada.Containers;

package body LBFGSB is

    function Minimise_LBFGSB (Fun  : Multilayer_Perceptron.Max_Function_Access;
                              X : Stochastic_Optimizers.Parameters_List;
                              Meth : Opt_Minimise.Method_Type;
                              Bounds : Constraints.Array_Bounds;
                              Max_Cor : Positive := 10;
                              Ftol    : Float :=
                                2.2204460492503131 * 10.0 ** (-09);
                              Gtol     : Float := 10.0 ** (-5);
                              Eps      : Float := 10.0 ** (-8);
                              Max_Fun  : Positive := 15000;
                              Max_Iter : Positive := 15000)
                             return Optimise.Optimise_Result is
        use Ada.Containers;
        use Stochastic_Optimizers;
        Routine_Name : constant String := "LBFGSB.Minimise_LBFGSB";
        X_Length     : constant Positive := Positive (X.Length);
        X_Clip       : Parameters_List := X;
        Result       : Optimise.Optimise_Result;
    begin
        --  L266
        Assert (Positive (Bounds.Lower.Length) = X_Length and
                  Positive (Bounds.Upper.Length) = X_Length, Routine_Name &
                  "Bounds and X have different lengths.");

--          for row in X_Clip.First_Index .. X_Clip.Last_Index loop
--              for col in X_Clip'Range (2) loop
--                  if X_Clip.Element (row). < Bounds.Lower.Element (row) then
--                      X_Clip (row) := Bounds.Lower.Element (row);
--                  elsif X_Clip (row) > Bounds.Upper.Element (row) then
--                      X_Clip (row) := Bounds.Upper.Element (row);
--                  end if;
--              end loop;
--          end loop;

        --  L309

        return Result;

    end Minimise_LBFGSB;

end LBFGSB;
