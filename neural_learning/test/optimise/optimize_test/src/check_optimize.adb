--  Based on /scipy/scipy/optimize/tests/test_optimize.py
--  CheckOptimize class

with Ada.Assertions; use Ada.Assertions;
with Ada.Text_IO; use Ada.Text_IO;

with Maths;

--  with Differentiable_Functions; use Differentiable_Functions;
--  with Num_Diff;
with Optimise;

package body Check_Optimize is
--     Epsilon : constant Float := 10.0 ** (-8);

   --  -------------------------------------------------------------------------

   function Der_Logit (X : Real_Float_Vector) return Real_Float_Vector is
      use Real_Float_Arrays;
      Exp_Val : constant Real_Float_Vector := Exp (-X);
      Result  : Real_Float_Vector := Exp_Val;
   begin
      for row in X'Range loop
            Result (row) := Exp_Val (row) / (1.0 + Exp_Val (row)) ** 2;
      end loop;

      return Result;

   end Der_Logit;

   --  -------------------------------------------------------------------------

   function Logit (X : Real_Float_Vector) return Real_Float_Vector is
      use Real_Float_Arrays;
      Exp_Val : constant Real_Float_Vector := Exp (-X);
      Result : Real_Float_Vector (X'Range);
   begin
      for row in X'Range loop
            Result (row) := 1.0 / (1.0 + Exp_Val (row));
      end loop;

      return Result;

   end Logit;

   --  -------------------------------------------------------------------------

--  L33 Test_Check_Grad verifes that check_grad can estimate the derivative of
--  the logistic function.
    procedure Test_Check_Grad is
--        use Real_Float_Arrays;
      Routine_Name : constant String := "Check_Optimize.Test_Check_Grad ";
        X0        : Real_Float_Vector (1 .. 1);
--          SF        : Scalar_Function (1, 1);
--          Fun       : constant RF_Fun_Access := null;
--          Deriv_Fun : Num_Diff.Deriv_Fun_Access;
        Grad_Func : constant Optimise.Grad_Func_Access := null;
        Result    : Float;
    begin
        X0 (1) := 1.5;
--          C_Init (SF, Fun, X0, Num_Diff.FD_None, Num_Diff.FD_None,
--                  Epsilon, Epsilon);
      Result := Optimise.Check_Grad (Der_Logit'Access, Grad_Func, X0);
      Put_Line (Routine_Name & "" & Float'Image (Result));

    end Test_Check_Grad;

    --  -------------------------------------------------------------------------

   function Func (Self : in out Check_Data; X : Real_Float_Vector)
                  return Float is
      use Real_Float_Arrays;
      use Maths.Float_Math_Functions;
      Routine_Name : constant String := "Check_Optimize.Func ";
      Log_PDot     : constant Real_Float_Vector := Self.F * X;
      PDot         : constant Real_Float_Vector := Exp (Log_PDot);
      PDot_Sum     : Float := 0.0;
   begin
      Self.Func_Calls := Self.Func_Calls + 1;
      Assert (Self.Func_Calls < 6000, Routine_Name &
                "too many iterations in optimization routine");
      for row in PDot'Range loop
         PDot_Sum := PDot_Sum + PDot (row);
      end loop;
      Self.Trace.Append (X);

      return Log (PDot_Sum) - Self.K * X;

   end Func;

   --  -------------------------------------------------------------------------

   function Grad (Self : in out Check_Data; X : Real_Float_Vector)
                  return Real_Float_Vector is
      use Real_Float_Arrays;
      use Maths.Float_Math_Functions;
--        Routine_Name : constant String := "Check_Optimize.Grad ";
      Log_PDot     : constant Real_Float_Vector := Self.F * X;
      PDot         : constant Real_Float_Vector := Exp (Log_PDot);
      PDot_Sum     : Float := 0.0;
   begin
      Self.Grad_Calls := Self.Grad_Calls + 1;
      for row in PDot'Range loop
         PDot_Sum := PDot_Sum + PDot (row);
      end loop;

      return Transpose (Self.F) * Exp (Log_PDot - Log (PDot_Sum));

   end Grad;

   --  -------------------------------------------------------------------------

   function Hess (Self : in out Check_Data; X : Real_Float_Vector)
                  return Real_Float_Matrix is
      use Real_Float_Arrays;
      use Maths.Float_Math_Functions;
--        Routine_Name : constant String := "Check_Optimize.Hess ";
      T_F          : constant Real_Float_Matrix := Transpose (Self.F);
      Log_PDot     : constant Real_Float_Vector := Self.F * X;
      PDot         : constant Real_Float_Vector := Exp (Log_PDot);
      PDot_Sum     : Float := 0.0;
      P            : Real_Float_Vector (PDot'Range);
      Diag_P       : Real_Float_Matrix (PDot'Range, PDot'Range) :=
                         (others => (others => 0.0));
   begin
      Self.Grad_Calls := Self.Grad_Calls + 1;
      for row in PDot'Range loop
         PDot_Sum := PDot_Sum + PDot (row);
      end loop;
      P := Exp (Log_PDot - Log (PDot_Sum));
      for index in P'Range loop
         Diag_P (index, index) := P (index);
      end loop;

      return T_F * (Diag_P * (T_F - P));

   end Hess;

   --  -------------------------------------------------------------------------

   function Hess_P (Self : in out Check_Data; X, P : Real_Float_Vector)
                  return Real_Float_Vector is
      use Real_Float_Arrays;
   begin
      return Hess (Self, X) * P;

   end Hess_P;

   --  -------------------------------------------------------------------------

end Check_Optimize;
