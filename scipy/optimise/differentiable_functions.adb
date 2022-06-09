--  Based on scipy/optimize/_differentiable_functions.py

with Ada.Assertions; use Ada.Assertions;

package body Differentiable_Functions is

    type Finite_Options is record
        Method            : Boolean;
        Rel_Step          : Boolean;
        Bounds            : Boolean;
        As_Linear_Opertor : Boolean;
    end record;


    procedure C_Init (Self : in out Scalar_Function;
                      fun : access function (X : Float) return Float;
                      X0   : Fortran_DP_Array; Grad, Hess : FD_Methods;
                      Finite_Diff_Rel_Step, Finite_Diff_Bounds : Float) is
        Routine_Name : constant String := "Differentiable_Functions.C_Init ";
    begin
--          Assert (not ((Grad in FD_Methods) and (Hess in FD_Methods)), Routine_Name &
--                    "Whenever the gradient is estimated via finite-differences" &
--                    " the Hessian must be estimated using one of the " &
--                    "quasi-Newton strategies.") ;
        null;
    end C_Init;

end Differentiable_Functions;
