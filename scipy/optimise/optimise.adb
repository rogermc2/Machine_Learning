--  Based on scipy/optimize/optimize.py

with Maths;

with Num_Diff;

package body Optimise is

    type BFGS_Options is (Gtol_Option, Norm_Option, Epsilon_Option, Disp_Option,
                          Max_Iter_Option, Return_All_Option);

    Epsilon : constant Float := Maths.Float_Math_Functions.Sqrt (Float'Safe_Last);

    --  ------------------------------------------------------------------------
    --  L1137 F_Min_BFGS minimizes a function using the BFGS algorithm.
    function F_Min_BFGS
      (F : RF_Fun_Access; X0 : Lbfgsb_F_Interface.Fortran_DP_Array)
     return Optimise_Result is
        Min_BFGS : Optimise_Result (0, 0, 0);
    begin

        return Min_BFGS;

    end F_Min_BFGS;

    --  ------------------------------------------------------------------------
    --  L1261
    function Minimise_BFGS
      (Fun : RF_Fun_Access; X0 : Lbfgsb_F_Interface.Fortran_DP_Array;
       Gtol : Float := 10.0 ** (-5); Norm : Float := Float'Safe_Last;
       Eps : Float := Epsilon; Max_Iter : Natural := 0;
       Disp, Return_All : Boolean := False) return Optimise_Result is
        Min_BFGS : Optimise_Result (0, 0, 0);
        Iters : Positive;
        Ret_All : Boolean := Return_All;
        SF    : Scalar_Function (X0'Length);
        F     : DP_Fun_Access;
        My_F_Prime : Num_Diff.FD_Methods;
        Old_Val    : Float;
        K          : Natural := 0;
    begin
        --  L1301
        if Max_Iter /= 0 then
            Iters := Max_Iter;
        else
            Iters := 200 * X0'Length;
        end if;

        SF := Prepare_Scalar_Function (Fun, X0);
        F := SF.Fun;
        My_F_Prime := SF.Grad;
        Old_Val := Float (F (X0));

        return Min_BFGS;

    end Minimise_BFGS;

    --  ------------------------------------------------------------------------

    function Prepare_Scalar_Function
      (Fun : RF_Fun_Access; X0 : Lbfgsb_F_Interface.Fortran_DP_Array;
       Bounds : Constraints.Array_Bounds := Constraints.Default_Bounds;
       Epsilon, Finite_Diff_Rel_Step : Float := 0.0)
       return Scalar_Function is
        SF : Scalar_Function (X0'Length);
    begin
        SF.Fun := Fun;
        SF.X := X0;
        SF.Bounds := Bounds;
        SF.F_Diff_Rel_Step := Finite_Diff_Rel_Step;
        SF.Epsilon := Epsilon;
        return SF;

    end Prepare_Scalar_Function;

    --  ------------------------------------------------------------------------

    function Prepare_Jac_Scalar_Function
      (Fun : RF_Fun_Access; X0, Jac : Lbfgsb_F_Interface.Fortran_DP_Array;
       Bounds : Constraints.Array_Bounds := Constraints.Default_Bounds;
       Epsilon, Finite_Diff_Rel_Step : Float := 0.0) return Scalar_Function is
        SF : Scalar_Function (1);
    begin
        SF.Fun := Fun;
        SF.X := X0;
        SF.Bounds := Bounds;
        SF.F_Diff_Rel_Step := Finite_Diff_Rel_Step;
        SF.Epsilon := Epsilon;
        SF.Jac := Jac;
        return SF;

    end Prepare_Jac_Scalar_Function;

    --  ------------------------------------------------------------------------

end Optimise;
