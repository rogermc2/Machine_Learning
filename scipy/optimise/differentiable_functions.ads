--  Based on scipy/optimize/_differentiable_functions.py

with Lbfgsb_F_Interface; use Lbfgsb_F_Interface;

package Differentiable_Functions is

    type FD_Methods is (FD_2_Point, FD_3_Point, FD_CS);

    --  The Scalar_Function class defines a scalar function F: R^n->R and
    --  methods for computing or approximating its first and second derivatives.
   type Scalar_Function (X_Size : Positive) is record
      fun       : access function (X: Float) return Float;
      X         : Fortran_DP_Array (1 .. X_Size);
      N_Fev     : Natural :=0;
      N_Gev     : Natural :=0;
      N_Hev     : Natural :=0;
      F_Updated : Boolean := False;
      G_Updated : Boolean := False;
      H_Updated : Boolean := False;
      Grad      : FD_Methods;
      Hess      : FD_Methods;
   end record;

   procedure C_Init (Self : in out Scalar_Function;
                     fun : access function (X : Float) return Float;
                     X0   : Fortran_DP_Array; Grad, Hess : FD_Methods;
                     Finite_Diff_Rel_Step, Finite_Diff_Bounds : Float);

end Differentiable_Functions;
