--  Based on scipy/optimize/optimize.py

with Maths;

with Num_Diff;

package body Optimise is

   --      type BFGS_Options is (Gtol_Option, Norm_Option, Epsilon_Option, Disp_Option,
   --                            Max_Iter_Option, Return_All_Option);

   Epsilon : constant Float := Maths.Float_Math_Functions.Sqrt (Float'Safe_Last);

   --  ------------------------------------------------------------------------
   --  L912 Approx_Fprime finds the finite difference approximation of the
   --  derivatives of a scalar or vector-valued function.
   --            f(xk[i] + epsilon[i]) - f(xk[i])
   --     f'[i] = ---------------------------------
   --                         epsilon[i]
   function Approx_Fprime
     (Xk      : Real_Float_Vector; Func : Num_Diff.Fun_Access;
      Epsilon : Real_Float_Vector)
      return Real_Float_Matrix is
      use Num_Diff;
      F0  : constant Real_Float_Vector := Func (Xk);

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
     (Fun       : RF_Fun_Access; Grad_Func : Grad_Func_Access;
      X0        : Real_Float_Vector; Epsilon : Float := 10.0 ** (-8);
      Direction : Direction_Kind := All_Direction) return Float is
      Step            : constant Float := Epsilon;
      Analytical_Grad : Float;
   begin
      case Direction is
         when Random_Direction => null;
         when All_Direction =>
            Analytical_Grad := Grad_Func (X0);
      end case;

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
     (Fun              : RF_Fun_Access; X0 : Real_Float_Vector;
      Gtol             : Float := 10.0 ** (-5); Norm        : Float := Float'Safe_Last;
      Eps              : Float := Epsilon; Max_Iter : Natural := 0;
      Disp, Return_All : Boolean := False) return Optimise_Result is
      Min_BFGS   : Optimise_Result (0, 0, 0);
      Iters      : Positive;
      Ret_All    : Boolean := Return_All;
      SF         : Scalar_Function (X0'Length);
      F          : RF_Fun_Access;
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
      Old_Val := F (X0);

      return Min_BFGS;

   end Minimise_BFGS;

   --  ------------------------------------------------------------------------

   function Prepare_Scalar_Function
     (Fun                           : RF_Fun_Access; X0       : Real_Float_Vector;
      Bounds                        : Constraints.Array_Bounds := Constraints.Default_Bounds;
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
     (Fun                           : RF_Fun_Access; X0, Jac  : Real_Float_Vector;
      Bounds                        : Constraints.Array_Bounds := Constraints.Default_Bounds;
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
