--  Based on scipy/optimize/_differentiable_functions.py

with Constraints;
with NL_Arrays_And_Matrices; use NL_Arrays_And_Matrices;
with Lbfgsb_F_Interface; use Lbfgsb_F_Interface;
with Num_Diff; use Num_Diff;

package Differentiable_Functions is

   type Finite_Options is private;
   type RF_Fun_Access is access
     function (X : Real_Float_Vector) return Float;

    --  The Scalar_Function class defines a scalar function F: R^n->R and
    --  methods for computing or approximating its first and second derivatives.
    type Scalar_Function (X_Size : Positive) is record
        Fun             : RF_Fun_Access;
        --        Update_Fun      : RF_Fun_Access;
        --        Update_Grad     : access procedure;
        X               : Real_Float_Vector (1 .. X_Size);
        Jac             : Real_Float_Vector (1 .. X_Size);
        Bounds          : Constraints.Array_Bounds;
        X_Prev          : Real_Float_Vector (1 .. X_Size) := (others => 0.0);
        G_Prev          : Real_Float_Vector (1 .. X_Size) := (others => 0.0);
        N_Fev           : Natural := 0;
        N_Gev           : Natural := 0;
        N_Hev           : Natural := 0;
        F_Updated       : Boolean := False;
        G_Updated       : Boolean := False;
        H_Updated       : Boolean := False;
        Grad            : FD_Methods;
        Hess            : FD_Methods;
        F_Diff_Rel_Step : Float := 0.0;
        F               : Float := Float'Safe_Last;
        G               : Real_Float_Vector (1 .. X_Size) := (others => 0.0);
        Lowest_X        : Real_Float_Vector (1 .. X_Size);
        Lowest_F        : Float := Float'Safe_Last;
        Epsilon         : Float := 10.0 ** (-8);
    end record;

    procedure C_Init
      (Self                   : in out Scalar_Function;
       Fun                    : RF_Fun_Access;
       X0                     : Real_Float_Vector; Grad, Hess : FD_Methods;
       Finite_Diff_Rel_Step,
       Finite_Diff_Bounds     : Float;
       Epsilon                : Float := 10.0 ** (-8));
    procedure Fun_And_Grad
      (Self : in out Scalar_Function;
       X    : Real_Float_Vector;
       Fun_Val : out Float; Grad : out Real_Float_Vector);
   function Grad (Self : in out Scalar_Function; X : Fortran_DP_Array)
                  return Real_Float_Vector;

private

    type Finite_Options is record
        Method             : FD_Methods;
        Rel_Step           : Float := 0.0;
        Abs_Step           : Float := 0.0;
        Bounds             : Float := 0.0;
        As_Linear_Operator : Boolean := False;
    end record;

end Differentiable_Functions;
