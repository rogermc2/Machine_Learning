--  Based on scipy/optimize/_optimize.py

with Constraints;
with Differentiable_Functions; use Differentiable_Functions;
with NL_Arrays_And_Matrices; use NL_Arrays_And_Matrices;
with Num_Diff;

package Optimise is

    type Direction_Kind is (Random_Direction, All_Direction);
    type Opt_Fun_Access is access function (X : Real_Float_List) return Float;
    type Lbfgs_Inv_Hess_Product (N_Coor, N : Positive) is record
        SK : NL_Arrays_And_Matrices.Real_Float_Matrix (1 .. N_Coor, 1 .. N);
        YK : NL_Arrays_And_Matrices.Real_Float_Matrix (1 .. N_Coor, 1 .. N);
    end record;
    --     type Grad_Func_Access is access function (X : Real_Float_Vector)
    --                                                return Real_Float_Matrix;

    type Optimise_Result
      (X_Length, N_Coor, N : Natural) is record
    --  Fun: values of objective function
        Fun       : Float;
        --  X: solution of the optimization.
        X         : Real_Float_Vector (1 .. X_Length);
        --  Jac: Jacobian of objective function
        Jac       : Real_Float_Vector (1 .. X_Length);
        --  Hess: Hessian of objective function
        Hess      : Real_Float_Vector (1 .. X_Length);
        Hess_Inv  : Lbfgs_Inv_Hess_Product (N_Coor, N);
        --  N_It: Number of iterations performed by the optimizer.
        N_It      : Natural := 0;
        --  Number of evaluations of the objective functions and of its
        --  Jacobian and Hessian:
        N_Fev     : Natural := 0;
        N_Jev     : Natural := 0;
        N_Hev     : Natural := 0;
        Status    : Natural := 0;
        Success   : Boolean := False;
    end record;

    function Check_Grad
      (Fun, Grad_Func : Num_Diff.Deriv_Fun_Access;
       X0             : Real_Float_Vector; Epsilon : Float := 10.0 ** (-8);
       Direction      : Direction_Kind := All_Direction) return Float;
    function Prepare_Scalar_Function
      (Fun                  : Num_Diff.Deriv_Float_Fun_Access; X0 : Real_Float_Vector;
       Bounds               : Constraints.Array_Bounds := Constraints.Default_Bounds;
       Epsilon,
       Finite_Diff_Rel_Step : Float := 10.0 ** (-8))
      return Scalar_Function;
    function Prepare_Jac_Scalar_Function
      (Fun                           : Num_Diff.Deriv_Float_Fun_Access;
       X0                            : Real_Float_Vector;
       Jac                           : Real_Float_Vector;
       Bounds                        : Constraints.Array_Bounds :=
         Constraints.Default_Bounds;
       Epsilon, Finite_Diff_Rel_Step : Float := 10.0 ** (-8))
      return Scalar_Function;
    function F_Min_BFGS (F : RF_Fun_Access; X0 : Real_Float_Vector)
                        return Optimise_Result;

end Optimise;
