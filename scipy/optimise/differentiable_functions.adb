--  Based on scipy/optimize/_differentiable_functions.py

with Ada.Assertions; use Ada.Assertions;

package body Differentiable_Functions is

   procedure Update_Fun (Self : in out Scalar_Function);
   procedure Update_Grad (Self : in out Scalar_Function);
   procedure Update_Grad_FD (Self : in out Scalar_Function);
   procedure Update_Hess (Self : in out Scalar_Function);
   procedure Update_X (Self : in out Scalar_Function;
                       Fun  : RF_Fun_Access; X : Real_Float_Vector);

   --  -------------------------------------------------------------------------

   procedure C_Init
     (Self                  : in out Scalar_Function;
      Fun                   : Deriv_Float_Fun_Access;
      X0                    : Real_Float_Vector; Grad, Hess : FD_Methods;
      Finite_Diff_Rel_Step,
      Finite_Diff_Bounds    : Float;
      Epsilon               : Float := 10.0 ** (-8)) is
      Routine_Name        : constant String :=
                              "Differentiable_Functions.C_Init ";
      Finite_Diff_Options : Finite_Options;
      pragma Unreferenced (Finite_Diff_Options);

   begin
      --  L100
      Assert (not (Grad = FD_None or Hess = FD_None), Routine_Name &
                "Whenever the gradient is estimated via finite-differences" &
                " the Hessian must be estimated using one of the " &
                "quasi-Newton strategies.") ;
      Self.Fun_Float := Fun;
      Self.X0 := X0;

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

      --  L140
      Update_Fun (Self);
      --  L143
      case Grad is
      when Fd_Callable =>
         Assert (False, Routine_Name & "Fd_Callable case not implemented.");
      when FD_2_Point | FD_3_Point | FD_CS =>
         Update_Grad_FD (Self);
      when others =>
         Assert (False, Routine_Name & "Invalid case.");
      end case;

      if Hess /= FD_None and Hess /= FD_Hessian_Update_Strategy then
         --  L212
         Update_Hess (Self);
         Self.H_Updated := True;
      elsif Hess = FD_Hessian_Update_Strategy then
         Self.Hess := Hess;
         Self.H_Updated := True;
         Self.X_Prev := Zero_Array (Positive (X0'Length));
      end if;

   end C_Init;

   --  -------------------------------------------------------------------------
   --  L264
   procedure Fun_And_Grad
     (Self    : in out Scalar_Function;
      X       : Real_Float_Vector;
      Fun_Val : out Float; Grad : out Real_Float_Vector) is
   begin
      Update_Fun (Self);
      Update_Grad (Self);
      Fun_Val := Self.Fun_Float (X);
      Grad := Self.G;

   end Fun_And_Grad;

   --  -------------------------------------------------------------------------

   --  L132
   function Fun_Wrapped (Self : in out Scalar_Function;
                         Fun  : Num_Diff.Deriv_Float_Fun_Access;
                         X    : Real_Float_Vector) return Float is
      --                            X    : Real_Float_Vector) return Real_Float_Matrix is
      FX : constant Float := Fun (X);
      --          FX : constant Real_Float_Matrix := Fun (X);
   begin
      --          Self.N_Fev := Self.N_Fev + 1;
      --          for index in FX'Range loop
      --              for col in FX'Range (2) loop
      --                  if FX (index, col) < Self.Lowest_F then
      --                      Self.Lowest_X := X;
      --                      Self.Lowest_F := FX (index, col);
      --                  end if;
      --              end loop;
      --          end loop;

      if FX < Self.Lowest_F then
         Self.Lowest_X := X;
         Self.Lowest_F := FX;
      end if;

      return FX;

   end Fun_Wrapped;

   --  -------------------------------------------------------------------------
   --  L270
   function Grad (Self : in out Scalar_Function; X : Real_Float_Vector)
                  return Real_Float_Vector is
   begin
      Update_Grad (Self);

      return Self.G;

   end Grad;

   --  -------------------------------------------------------------------------

   procedure Update_Fun (Self : in out Scalar_Function) is
   begin
      Self.F := Fun_Wrapped (Self, Self.Fun_Float, Self.X0);
   end Update_Fun;

   --  -------------------------------------------------------------------------
   --  L239
   procedure Update_Grad (Self : in out Scalar_Function) is
      --        F0  : Real_Float_Vector (1 .. 1) := (1 => Self.F);
   begin
      if not Self.G_Updated then
         --           Self.Update_Grad_Impl;
         Self.G_Updated := True;
      end if;

   end Update_Grad;

   --  -------------------------------------------------------------------------
   --  L152 for grad in FD_METHODS
   procedure Update_Grad_FD (Self : in out Scalar_Function) is
      Fun : Deriv_Fun_Access;
      F0  : Real_Float_Vector (1 .. 1) := (1 => Self.F);
   begin
      Update_Fun (Self);
      Self.N_Gev := Self.N_Gev + 1;
--        Self.G := Num_Diff.Approx_Derivative
--          (Fun, Self.X0, Abs_Step => Self.Epsilon, F0 => F0);

   end Update_Grad_FD;

   --  -------------------------------------------------------------------------

   procedure Update_Hess (Self : in out Scalar_Function) is
   begin
      null;

   end Update_Hess;

   --  -------------------------------------------------------------------------

   procedure Update_X (Self : in out Scalar_Function; Fun  : RF_Fun_Access;
                       X    : Real_Float_Vector) is
   begin
      Self.X0 := X;
      Self.F_Updated := False;
      Self.G_Updated := False;
      Self.H_Updated := False;

   end Update_X;

   --  -------------------------------------------------------------------------

end Differentiable_Functions;
