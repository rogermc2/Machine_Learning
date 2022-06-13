--  Based on scipy/optimize/_differentiable_functions.py

with Lbfgsb_F_Interface; use Lbfgsb_F_Interface;
with Num_Diff; use Num_Diff;

package Differentiable_Functions is

   type Finite_Options is private;

   --  The Scalar_Function class defines a scalar function F: R^n->R and
   --  methods for computing or approximating its first and second derivatives.
   type Scalar_Function (X_Size : Positive) is record
      Fun             : access function (X: Fortran_DP_Array) return Float;
      --          Update_Fun_Impl  : access function (X: Fortran_DP_Array) return Float;
      --          Update_Grad_Impl : access procedure;
      X               : Fortran_DP_Array (1 .. X_Size);
      N_Fev           : Natural := 0;
      N_Gev           : Natural := 0;
      N_Hev           : Natural := 0;
      F_Updated       : Boolean := False;
      G_Updated       : Boolean := False;
      H_Updated       : Boolean := False;
      Grad            : FD_Methods;
      Hess            : FD_Methods;
      F               : Float := Float'Safe_Last;
      Lowest_X        : Fortran_DP_Array (1 .. X_Size);
      Lowest_F        : Float := Float'Safe_Last;
   end record;

   procedure C_Init
     (Self                                     : in out Scalar_Function;
      Fun                                      : access function (X : Fortran_DP_Array) return Float;
      X0                                       : Fortran_DP_Array; Grad, Hess      : FD_Methods;
      Finite_Diff_Rel_Step, Finite_Diff_Bounds : Float);

private

   type Finite_Options is record
      Method            : FD_Methods;
      Rel_Step          : Float := 0.0;
      Bounds            : Float := 0.0;
      As_Linear_Opertor : Boolean;
   end record;

end Differentiable_Functions;
