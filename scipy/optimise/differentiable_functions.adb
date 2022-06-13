--  Based on scipy/optimize/_differentiable_functions.py

with Ada.Assertions; use Ada.Assertions;

package body Differentiable_Functions is

   procedure C_Init
     (Self                  : in out Scalar_Function;
      Fun                   : DP_Fun_Access;
      X0                    : Fortran_DP_Array; Grad, Hess      : FD_Methods;
      Finite_Diff_Rel_Step,
      Finite_Diff_Bounds    : Float;
      Epsilon               : Float := 10.0 ** (-8)) is
      Routine_Name        : constant String := "Differentiable_Functions.C_Init ";
      Finite_Diff_Options : Finite_Options;

      --  L132
      function Fun_Wrapped (X : Fortran_DP_Array) return Float is
         FX : constant Float := Fun (X);
      begin
         Self.N_Fev := Self.N_Fev + 1;
         if FX < Self.Lowest_F then
            Self.Lowest_X := X;
            Self.Lowest_F := FX;
         end if;

         return FX;

      end Fun_Wrapped;

      procedure Update_Fun is
      begin
         Self.F := Fun_Wrapped (Self.X);
      end Update_Fun;

      --  L170
      procedure Update_Grad is
      begin
         Update_Fun;
         Self.N_Gev := Self.N_Gev + 1;
--           Self.Grad := Num_Diff.Approx_Derivative
--             (Fun'Access, Self.X, Abs_Step => Epsilon, F0 => Self.F);

      end Update_Grad;

      procedure Update_Hess is
      begin
         null;

      end Update_Hess;

      procedure Update_X (X : Fortran_DP_Array) is
      begin
         Self.X := X;
         Self.F_Updated := False;
         Self.G_Updated := False;
         Self.H_Updated := False;

      end Update_X;

   begin
      --  L100
      Assert (not (Grad = FD_None or Hess = FD_None), Routine_Name &
                "Whenever the gradient is estimated via finite-differences" &
                " the Hessian must be estimated using one of the " &
                "quasi-Newton strategies.") ;
      Self.X := X0;

      --  L120
      Finite_Diff_Options.Rel_Step := Finite_Diff_Rel_Step;
      Finite_Diff_Options.Abs_Step := Epsilon;
      if Grad /= FD_None then
         Finite_Diff_Options.Method := Grad;
         Finite_Diff_Options.Bounds := Finite_Diff_Bounds;
      elsif Hess /= FD_None then
         Finite_Diff_Options.Method := Hess;
         Finite_Diff_Options.As_Linear_Operator := True;
      end if;

      --  L158
      Update_Fun;
      --  L177
      Update_Grad;

      if Hess /= FD_None then
         --  L212
         Update_Hess;
         Self.H_Updated := True;
      end if;

   end C_Init;

end Differentiable_Functions;
