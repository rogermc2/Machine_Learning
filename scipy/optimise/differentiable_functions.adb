--  Based on scipy/optimize/_differentiable_functions.py

with Ada.Assertions; use Ada.Assertions;

package body Differentiable_Functions is

    procedure C_Init
      (Self : in out Scalar_Function;
       Fun : access function (X : Fortran_DP_Array) return Float;
       X0   : Fortran_DP_Array; Grad, Hess : FD_Methods;
       Finite_Diff_Rel_Step, Finite_Diff_Bounds : Float) is
        Routine_Name : constant String := "Differentiable_Functions.C_Init ";
        Finite_Diff_Options : Finite_Options;

        function Fun_Wrapped (X : Fortran_DP_Array) return Float is
        begin
            Self.N_Fev := Self.N_Fev + 1;
            return Fun (X);

        end Fun_Wrapped;

        procedure Update_Fun is
        begin
            Self.Fun := Fun_Wrapped (Self.X);
        end Update_Fun;

        procedure Update_Grad is
        begin
            Update_Fun;

        end Update_Grad;

    begin
        Assert (not (Grad = FD_None or Hess = FD_None), Routine_Name &
                  "Whenever the gradient is estimated via finite-differences" &
                  " the Hessian must be estimated using one of the " &
                  "quasi-Newton strategies.") ;
        Finite_Diff_Options.Rel_Step := Finite_Diff_Rel_Step;
        if Grad /= FD_None then
            Finite_Diff_Options.Method := Grad;
            Finite_Diff_Options.Bounds := Finite_Diff_Bounds;
        elsif Hess /= FD_None then
            Finite_Diff_Options.Method := Hess;
            Finite_Diff_Options.As_Linear_Opertor := True;
        end if;

        Update_Fun;

        if Grad /= FD_None then
            Self.Update_Grad_Impl := Update_Grad'Access;
        end if;

    end C_Init;

end Differentiable_Functions;
