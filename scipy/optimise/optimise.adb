--  Based on scipy/optimize/optimize.py

with Ada.Assertions; use Ada.Assertions;
with Maths;

package body Optimise is

    --      type BFGS_Options is (Gtol_Option, Norm_Option, Epsilon_Option, Disp_Option,
    --                            Max_Iter_Option, Return_All_Option);

    Epsilon : constant Float := Maths.Float_Math_Functions.Sqrt (Float'Safe_Last);

    --  ------------------------------------------------------------------------
    --  L912 Approx_Fprime finds the finite difference approximation of the
    --  derivatives of a scalar or vector-valued function .
    --            f(xk[i] + epsilon[i]) - f(xk[i])
    --     f'[i] = ---------------------------------
    --                         epsilon[i]
    --  If a function maps from R^n to R^m its derivatives form an m-by-n matrix
    --  called the Jacobian where an element (i, j) is a partial derivative of
    --  f[i] with respect to x[j].
    --  Approx_Fprime returns Func's partial derivatives with respect to Xk.
    function Approx_Fprime
      (Xk : Real_Float_Vector; Func : Grad_Func_Access;
       Epsilon : Real_Float_Vector)
       return Real_Float_Matrix is
        use Num_Diff;
        F0  : constant Real_Float_Matrix := Func (Xk);

    begin
        return Approx_Derivative
          (Fun => Func, X0 => Xk, Method => FD_2_Point, Abs_Step => Epsilon,
           F0  => F0);

    end Approx_Fprime;

    --  ------------------------------------------------------------------------
    --  L985  Check_Grad checks the correctness of a gradient function by
    --  comparing it against a (forward) finite-difference approximation of the
    --  gradient.
    function Check_Grad
      (Fun, Grad_Func : Grad_Func_Access;
       X0        : Real_Float_Vector; Epsilon : Float := 10.0 ** (-8);
       Direction : Direction_Kind := All_Direction) return Float is
        use Real_Float_Arrays;
        use Num_Diff;
        Routine_Name : constant String := "Optimise.Check_Grad ";
        Step            : Real_Float_Vector (1 .. 1);
        Analytical_Grad : Real_Float_Matrix (X0'Range, 1 .. 1);
        Diff            : Real_Float_Matrix (X0'Range, 1 .. 1);
    begin
        Assert (Fun /= null, Routine_Name & "Fun is null");
        Assert (Grad_Func /= null, Routine_Name & "Grad_Func is null");
        Step (1) := Epsilon;
        case Direction is
            when Random_Direction => null;
            when All_Direction =>
                Analytical_Grad := Grad_Func (X0);
        end case;

        Diff := Analytical_Grad - Approx_Fprime (X0, Fun, Step);

        return 0.0;

    end Check_Grad;

    --  ------------------------------------------------------------------------
    --  L1137 F_Min_BFGS minimizes a function using the BFGS algorithm.
    function F_Min_BFGS
      (F : RF_Fun_Access; X0 : Real_Float_Vector) return Optimise_Result is
        Min_BFGS : Optimise_Result (0, 0, 0);
    begin

        return Min_BFGS;

    end F_Min_BFGS;

    --  ------------------------------------------------------------------------
    --  L1261
    function Minimise_BFGS
      (Fun              : Num_Diff.Deriv_Fun_Access; X0 : Real_Float_Vector;
       Gtol             : Float := 10.0 ** (-5);
       Norm        : Float := Float'Safe_Last;
       Eps              : Float := Epsilon; Max_Iter : Natural := 0;
       Disp, Return_All : Boolean := False) return Optimise_Result is
        Min_BFGS   : Optimise_Result (0, 0, 0);
        Iters      : Positive;
        Ret_All    : Boolean := Return_All;
        SF         : Scalar_Function (X0'Length, 1);
        F          : Num_Diff.Deriv_Fun_Access;
        My_F_Prime : Num_Diff.FD_Methods;
        Old_Val    : Real_Float_Vector (X0'Range);
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
        Old_Val := F (X0);

        return Min_BFGS;

    end Minimise_BFGS;

    --  ------------------------------------------------------------------------

    function Prepare_Scalar_Function
      (Fun : Num_Diff.Deriv_Fun_Access;
       X0 : Real_Float_Vector;
       Bounds               : Constraints.Array_Bounds :=
         Constraints.Default_Bounds;
       Epsilon,
       Finite_Diff_Rel_Step : Float := 10.0 ** (-8))
       return Scalar_Function is
        SF : Scalar_Function (X0'Length, 1);
    begin
        SF.Fun := Fun;
        SF.X0 := X0;
        SF.Bounds := Bounds;
        SF.F_Diff_Rel_Step := Finite_Diff_Rel_Step;
        for index in SF.Epsilon'Range loop
            SF.Epsilon (index) := Epsilon;
        end loop;
        return SF;

    end Prepare_Scalar_Function;

    --  ------------------------------------------------------------------------

    function Prepare_Jac_Scalar_Function
      (Fun     : Num_Diff.Deriv_Fun_Access;
       X0, Jac  : Real_Float_Vector;
       Bounds                        : Constraints.Array_Bounds :=
         Constraints.Default_Bounds;
       Epsilon, Finite_Diff_Rel_Step : Float := 10.0 ** (-8))
       return Scalar_Function is
        SF : Scalar_Function (X0'Length, 1);
    begin
        SF.Fun := Fun;
        SF.X0 := X0;
        SF.Bounds := Bounds;
        SF.F_Diff_Rel_Step := Finite_Diff_Rel_Step;
        SF.Jac := Jac;
        for index in SF.Epsilon'Range loop
            SF.Epsilon (index) := Epsilon;
        end loop;

        return SF;

    end Prepare_Jac_Scalar_Function;

    --  ------------------------------------------------------------------------

end Optimise;
