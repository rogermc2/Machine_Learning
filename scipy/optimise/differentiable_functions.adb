--  Based on scipy/optimize/_differentiable_functions.py

with Ada.Assertions; use Ada.Assertions;
with Ada.Text_IO; use Ada.Text_IO;

package body Differentiable_Functions is

   procedure Update_Fun (Self : in out Scalar_Function;
                         Args : Multilayer_Perceptron.Loss_Grad_Args);
   procedure Update_Grad (Self : in out Scalar_Function;
                         Args : Multilayer_Perceptron.Loss_Grad_Args);
   procedure Update_Grad_FD (Self : in out Scalar_Function;
                         Args : Multilayer_Perceptron.Loss_Grad_Args);
   procedure Update_Hess (Self : in out Scalar_Function;
                         Args : Multilayer_Perceptron.Loss_Grad_Args);
   procedure Update_X (Self : in out Scalar_Function;
                       Fun  : RF_Fun_Access;
                       Args : Multilayer_Perceptron.Loss_Grad_Args;
                       X    : Real_Float_Vector);

   --  -------------------------------------------------------------------------

   procedure C_Init
     (Self                  : in out Scalar_Function;
      Fun                   : Multilayer_Perceptron.Loss_Grad_Access;
      Args                  : Multilayer_Perceptron.Loss_Grad_Args;
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
      Update_Fun (Self, Args);
      --  L143
      case Grad is
      when Fd_Callable =>
         Assert (False, Routine_Name & "Fd_Callable case not implemented.");
      when FD_2_Point | FD_3_Point | FD_CS =>
         Update_Grad_FD (Self, Args);
      when others =>
         Assert (False, Routine_Name & "Invalid case.");
      end case;

      if Hess /= FD_None and Hess /= FD_Hessian_Update_Strategy then
         --  L212
         Update_Hess (Self, Args);
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
      Args    : Multilayer_Perceptron.Loss_Grad_Args;
      Fun_Val : out Float; Grad : out Real_Float_Vector) is
      --  Routine_Name : constant String := "Differentiable_Functions.Fun_And_Grad ";
   begin
      Update_Fun (Self, Args);
      Update_Grad (Self, Args);
      Fun_Val := Self.Fun_Float (Args).Loss;
      Grad := Self.G;

   end Fun_And_Grad;

   --  -------------------------------------------------------------------------

   --  L132
   function Fun_Wrapped (Self : in out Scalar_Function;
                         Args : Multilayer_Perceptron.Loss_Grad_Args;
                         X    : Real_Float_Vector) return Float is
--        Routine_Name : constant String := "Differentiable_Functions.Fun_Wrapped ";
      FX : Float;
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

--        Put_Line (Routine_Name);
--        Put_Line (Routine_Name & "Args.X'Length" & Integer'Image (Args.X'Length));
--        Put_Line (Routine_Name & "Args.Y'Length" & Integer'Image (Args.Y'Length));
--        Put_Line (Routine_Name & "Args.Params Length" &
--                    Integer'Image (Integer (Args.Params.Length)));
--        Put_Line (Routine_Name & "Args.Activations Length" &
--                    Integer'Image (Integer (Args.Activations.Length)));
--        Put_Line (Routine_Name & "Args.Gradients Length" &
--                    Integer'Image (Integer (Args.Gradients.Length)));
      FX := Self.Fun_Float (Args).Loss;
--        Put_Line (Routine_Name & "FX set");
      if FX < Self.Lowest_F then
         Self.Lowest_X := X;
         Self.Lowest_F := FX;
      end if;

      return FX;

   end Fun_Wrapped;

   --  -------------------------------------------------------------------------
   --  L270
   function Grad (Self : in out Scalar_Function;
                  Args : Multilayer_Perceptron.Loss_Grad_Args;
                  X    : Real_Float_Vector)
                  return Real_Float_Vector is
      use Real_Float_Arrays;
      Routine_Name : constant String := "Differentiable_Functions.Grad ";

   begin
      if X /= Self.X0 then
            Put_Line (Routine_Name & "X /= Self.X0");
      end if;
      Update_Grad (Self, Args);

      return Self.G;

   end Grad;

   --  -------------------------------------------------------------------------

   procedure Update_Fun (Self : in out Scalar_Function;
                         Args : Multilayer_Perceptron.Loss_Grad_Args) is
      --  Routine_Name : constant String := "Differentiable_Functions.Update_Fun ";
      FW           : Float;
   begin
      FW := Fun_Wrapped (Self, Args, Self.X0);
      Self.F := FW;
   end Update_Fun;

   --  -------------------------------------------------------------------------
   --  L239
   procedure Update_Grad (Self : in out Scalar_Function;
                         Args : Multilayer_Perceptron.Loss_Grad_Args) is
      --        F0  : Real_Float_Vector (1 .. 1) := (1 => Self.F);
      --  Routine_Name : String := "Differentiable_Functions.Update_Grad";
   begin
      if not Self.G_Updated then
         --           Self.Update_Grad_Impl;
         Self.G_Updated := True;
      end if;

   end Update_Grad;

   --  -------------------------------------------------------------------------
   --  L152 for grad in FD_METHODS
   procedure Update_Grad_FD (Self : in out Scalar_Function;
                             Args : Multilayer_Perceptron.Loss_Grad_Args) is
      Fun : Deriv_Fun_Access;
      F0  : Real_Float_Vector (1 .. 1) := (1 => Self.F);
   begin
      Update_Fun (Self, Args);
      Self.N_Gev := Self.N_Gev + 1;
--        Self.G := Num_Diff.Approx_Derivative
--          (Fun, Self.X0, Abs_Step => Self.Epsilon, F0 => F0);

   end Update_Grad_FD;

   --  -------------------------------------------------------------------------

   procedure Update_Hess (Self : in out Scalar_Function;
                         Args : Multilayer_Perceptron.Loss_Grad_Args) is
   begin
      null;

   end Update_Hess;

   --  -------------------------------------------------------------------------

   procedure Update_X (Self : in out Scalar_Function; Fun : RF_Fun_Access;
                        Args : Multilayer_Perceptron.Loss_Grad_Args;
                       X    : Real_Float_Vector) is
   begin
      Self.X0 := X;
      Self.F_Updated := False;
      Self.G_Updated := False;
      Self.H_Updated := False;

   end Update_X;

   --  -------------------------------------------------------------------------

end Differentiable_Functions;
