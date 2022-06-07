--  Based on scipy/optimize/lbfgsb_py.py

with Ada.Assertions; use Ada.Assertions;
with Ada.Containers;

package body LBFGSB is

    function Minimise_LBFGSB (Fun  : Multilayer_Perceptron.Max_Function_Access;
                              X : Stochastic_Optimizers.Parameters_List;
                              Meth : Opt_Minimise.Method_Type;
                              Bounds : Constraints.Array_Bounds)
                             return Optimise.Optimise_Result is
        use Ada.Containers;
        use Stochastic_Optimizers;
        Routine_Name : constant String := "LBFGSB.Minimise_LBFGSB";
        X_Length     : constant Positive := Positive (X.Length);
        X_Clip       : Parameters_List := X;
        Result       : Optimise.Optimise_Result;
    begin
        Assert (Positive (Bounds.Lower.Length) = X_Length and
                  Positive (Bounds.Upper.Length) = X_Length, Routine_Name &
                  "Bounds and X have different lengths.");

        for row in X_Clip.First_Index .. X_Clip.Last_Index loop
            for col in X_Clip'Range (2) loop
                if X_Clip (row, col) < Bounds (row, 1) then
                    X_Clip (row, col) := Bounds (row, 1);
                elsif X_Clip (row, col) > Bounds (row, 2) then
                    X_Clip (row, col) := Bounds (row, 2);
                end if;
            end loop;
        end loop;

        return Result;

    end Minimise_LBFGSB;

end LBFGSB;
