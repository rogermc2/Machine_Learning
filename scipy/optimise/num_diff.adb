--  Based on scipy/optimize/_numdiff.py

with Ada.Assertions; use Ada.Assertions;
with Ada.Containers;
--  with Ada.Text_IO; use Ada.Text_IO;

with Maths;

package body Num_Diff is

   type Scheme_Type is (One_Sided, Two_Sided);
   --      type Wrapped_Access is
   --        access function (X : Real_Float_Vector) return Real_Float_Vector;

   EPS : constant Float := Float'Small;

   function Check_Bounds
     (X0 : Real_Float_Vector; Bounds : Constraints.Bounds_List)
       return Boolean;
   function Check_Bounds
     (X0 : Real_Float_Vector; Bounds : Constraints.Bounds_List)
       return Boolean_Array;
   function Compute_Absolute_Step
     (Rel_Step : in out Real_Float_List; X0 : Real_Float_Vector;
      Method   : FD_Methods) return Real_Float_Vector;
   function Dense_Difference (W_Fun            : Deriv_Fun_Access;
                              X0               : Real_Float_Vector;
                              F0               : Real_Float_Vector;
                              H, Use_One_Sided : Real_Float_Vector;
                              Method           : FD_Methods)
                              return Real_Float_Matrix;
   function EPS_For_Method (Method : FD_Methods) return Float;
   function Inf_Bounds (Bounds : Constraints.Bounds_List) return Boolean;
   function Fun_Wrapped (Fun : Deriv_Fun_Access; X : Real_Float_Vector)
                          return Real_Float_Vector;
   --     function Linear_Operator_Difference
   --       (Fun    : Deriv_Fun_Access; X0, F0 : Real_Float_Vector; H : Real_Float_List;
   --        Method : FD_Methods) return Real_Float_Vector;
   function Mat_Vec (Fun    : Deriv_Fun_Access; X0 : Real_Float_Vector;
                     F0     : Real_Float_Vector; H : Real_Float_Vector;
                     Method : FD_Methods)
                      return Real_Float_Matrix;
   function Prepare_Bounds (Bounds : Constraints.Bounds_List;
                            X0     : Real_Float_Vector)
                             return Constraints.Bounds_List;
   function Relative_Step (Method : FD_Methods) return Float;

   --  -------------------------------------------------------------------------

   procedure Adjust_Scheme_To_Bounds
     (X0            : Real_Float_Vector; H : in out Real_Float_Vector;
      Num_Steps     : Positive; Scheme : Scheme_Type;
      Bounds        : Constraints.Bounds_List;
      Use_One_Sided : in out Real_Float_Vector) is
      use Real_Float_Arrays;
      use Constraints;
      use Constraints.Array_Bounds_Package;
      Lower         : constant Real_Float_Vector := Get_Lower (Bounds);
      Upper         : constant Real_Float_Vector := Get_Upper (Bounds);
      All_Inf       : Boolean := False;
      Lower_Dist    : Real_Float_Vector (H'Range);
      Upper_Dist    : Real_Float_Vector (H'Range);
      Min_Dist      : Real_Float_Vector (H'Range);
      H_Total       : Real_Float_Vector (H'Range);
      X             : Real_Float_Vector (X0'Range);
      H_Adjusted    : Real_Float_Vector (H'Range);
      Violated      : Boolean_Array (H_Total'Range);
      Fitting       : Boolean_Array (H_Total'Range);
      V_F           : Boolean_Array (H_Total'Range);
      Forward       : Boolean_Array (H_Total'Range);
      Backward      : Boolean_Array (H_Total'Range);
      Central       : Boolean_Array (H_Total'Range);
      Adjusted_Cent : Boolean_Array (H_Total'Range);
   begin
      H_Adjusted := H;
      case Scheme is
         when One_Sided => Use_One_Sided := (others => 1.0);
         when Two_Sided =>
            H := abs (H);
            Use_One_Sided := (others => 0.0);
      end case;

      for index in Lower'Range loop
         All_Inf := All_Inf or
           Lower (index) = Float'Safe_First or
           Upper (index) = Float'Safe_Last;
      end loop;

      if not All_Inf then
         H_Total := Float (Num_Steps) * H;
         Lower_Dist := X0 - Lower;
         Upper_Dist := Upper - X0;
      end if;

      --  L63
      case Scheme is
         when One_Sided =>
            X := X0 + H_Total;
            Violated := Check_Bounds (X, Bounds);
            Fitting := abs (H_Total) <= Max (Lower_Dist, Upper_Dist);
            V_F := Violated and Fitting;
            --  L61
            for index in H_Adjusted'Range loop
               if V_F (index) then
                  H_Adjusted (index) := -1.0 * H_Adjusted (index);
               end if;
            end loop;

            Fitting := not Fitting;
            Forward := (Upper_Dist >= Lower_Dist) and Fitting;

            for index in H_Adjusted'Range loop
               if Forward (index) then
                  H_Adjusted (index) :=
                    Upper_Dist (index) / Float (Num_Steps);
               end if;
            end loop;

            Backward := (Upper_Dist < Lower_Dist) and Fitting;
            for index in H_Adjusted'Range loop
               if Backward (index) then
                  H_Adjusted (index) :=
                    -Lower_Dist (index) / Float (Num_Steps);
               end if;
            end loop;

         when Two_Sided =>
            Central := Lower_Dist >= H_Total;
            Forward := (Upper_Dist >= Lower_Dist) and not Central;
            for index in H_Adjusted'Range loop
               if Forward (index) then
                  H_Adjusted (index) :=
                    Float'Min (H (index), 0.5 * Upper_Dist (index)) /
                      Float (Num_Steps);
                  Use_One_Sided (index) := 1.0;
               end if;
            end loop;

            Backward := (Upper_Dist < Lower_Dist) and not Central;
            for index in H_Adjusted'Range loop
               if Backward (index) then
                  H_Adjusted (index) :=
                    -Float'Min (H (index), 0.5 * Lower_Dist (index)) /
                    Float (Num_Steps);
                  Use_One_Sided (index) := 1.0;
               end if;
            end loop;

            Min_Dist := Min (Upper_Dist, Lower_Dist);
            Adjusted_Cent := (not Central) and (abs (H_Adjusted) <= Min_Dist);
            for index in H_Adjusted'Range loop
               if Adjusted_Cent (index) then
                  Use_One_Sided (index) := 0.0;
               end if;
            end loop;

      end case;

      H := H_Adjusted;

   end Adjust_Scheme_To_Bounds;

   --  -------------------------------------------------------------------------
   --  L257 Approx_Derivative computes the finite difference approximation of
   --  the derivatives of a vector-valued function.
   --  If a function maps from R^n to R^m its derivatives form m-by-n matrix
   --  called the Jacobian where an element (i, j) is a partial derivative of
   --  f[i] with respect to x[j].
   function Approx_Derivative
     (Fun                : Deriv_Fun_Access; X0 : Real_Float_Vector;
      Method             : FD_Methods := FD_None;
      Rel_Step           : Real_Float_List := Real_Float_Package.Empty_Vector;
      Abs_Step           : Real_Float_Vector;
      F0                 : Real_Float_Vector;
      Bounds             : Constraints.Bounds_List :=
        Constraints.Array_Bounds_Package.Empty_Vector;
      As_Linear_Operator : Boolean := False) return Real_Float_Vector is
      use  Ada.Containers;
      use Real_Float_Arrays;
      Routine_Name  : constant String := "Num_Diff.Approx_Derivative ";
      Loc_Bounds    : constant Constraints.Bounds_List :=
                        Prepare_Bounds (Bounds, X0);
      L_Rel_Step    : Real_Float_List := Rel_Step;
      L_F0          : Real_Float_Vector := F0;
      Use_One_Sided : Real_Float_Vector (X0'Range) := (others => 0.0);
      H             : Real_Float_Vector (X0'Range);
      Sign_X0       : Real_Float_Vector (X0'Range) := X0 >= 0.0;
      dX            : Real_Float_Vector (X0'Range);
      dF_dX         : Real_Float_Matrix (X0'Range, 1 .. 1);
      pragma Unreferenced (dF_dX);

   begin
      --  L428
      Assert (Loc_Bounds.Length = X0'Length, Routine_Name &
                "Bounds and X0 lengths unequal.");
      if As_Linear_Operator then
         Assert (Inf_Bounds (Bounds), Routine_Name &
                   "Bounds not supported for Linear_Operator.");
      end if;

      --  L443
      if F0'Last < F0'First then
         --  F0 is none
         L_F0 := Fun_Wrapped (Fun, X0);
      end if;

      Assert (Check_Bounds (X0, Bounds), Routine_Name & "invalid X0 Bounds");

      --  L472
      if As_Linear_Operator then
         --  when As_Linear_Operator is True Approx_Derivative should return a
         --  LinearOperator (df_dx)
         if L_Rel_Step.Is_Empty then
            L_Rel_Step (1) := Relative_Step (Method);
         end if;
         --           Result := Linear_Operator_Difference
         --             (Fun_Wrapped'Access, X0, F0, L_Rel_Step, Method);
      else
         --  L480
         if Abs_Step'Last < Abs_Step'First then
            H := Compute_Absolute_Step (L_Rel_Step, X0, Method);
         else
            Sign_X0 := 2.0 * Sign_X0 - 1.0;
            dX := (X0 + Abs_Step) - X0;
            H := Abs_Step;
            for row in H'Range loop
               if dX (row) = 0.0 then
                  H (row) := EPS_For_Method (Method) *
                    Float'Max (1.0, abs (X0 (row)));
               end if;
            end loop;
         end if;

         df_dx := Mat_Vec (Fun, X0, F0, H, Method);

         --  L495
         case Method is
            when FD_2_Point =>
               Adjust_Scheme_To_Bounds
                 (X0, H, 1, One_Sided, Bounds, Use_One_Sided);
            when FD_3_Point =>
               Adjust_Scheme_To_Bounds
                 (X0, H, 1, Two_Sided, Bounds, Use_One_Sided);
            when FD_CS => Use_One_Sided := (others => 0.0);
            when FD_None | Fd_Callable | FD_True | FD_False |
                 FD_Hessian_Update_Strategy => null;
         end case;
      end if;

      --  L504 if sparsity is None:
      return Dense_Difference (Fun, X0, F0, H, Use_One_Sided, Method);

   end Approx_Derivative;

   --  -------------------------------------------------------------------------

   function Check_Bounds
     (X0 : Real_Float_Vector; Bounds : Constraints.Bounds_List)
       return Boolean_Array is
      Result : Boolean_Array (X0'Range);
   begin
      for index in  X0'Range loop
         Result (index) := X0 (index) > Bounds.Element (index).Lower and
           X0 (index) < Bounds.Element (index).Upper;
      end loop;

      return Result;

   end Check_Bounds;

   --  -------------------------------------------------------------------------

   function Check_Bounds
     (X0 : Real_Float_Vector; Bounds : Constraints.Bounds_List)
       return Boolean is
      Result : Boolean := True;
   begin
      for index in  X0'Range loop
         Result := Result and X0 (index) > Bounds.Element (index).Lower and
           X0 (index) < Bounds.Element (index).Upper;
      end loop;

      return Result;

   end Check_Bounds;

   --  -------------------------------------------------------------------------
   --  L144
   function Compute_Absolute_Step
     (Rel_Step : in out Real_Float_List; X0 : Real_Float_Vector;
      Method   : FD_Methods) return Real_Float_Vector is
      use Real_Float_Arrays;
      R_Step   : constant Float := EPS_For_Method (Method);
      Sign_X0  : Real_Float_Vector (X0'Range) := X0 >= 0.0;
      Abs_Step : Real_Float_Vector (X0'Range) := R_Step * Sign_X0;
      dX       : Real_Float_Vector (X0'Range);
   begin
      --  L170  this is used because Sign_X0 needs to be 1 when x0 = 0.
      Sign_X0 := 2.0 * Sign_X0 - 1.0;

      if Rel_Step.Is_Empty then
         for row in Abs_Step'Range loop
            Abs_Step (row) := Abs_Step (row)  + Float'Max (1.0, abs (X0 (row)));
         end loop;
      else
         Abs_Step := Abs_Step * abs (X0);
      end if;

      dX := (X0 + Abs_Step) - X0;
      for row in Abs_Step'Range loop
         if dX (row) = 0.0 then
            Abs_Step (row) := R_Step * Sign_X0 (row) *
              Float'Max (1.0, abs (X0 (row)));
         end if;
      end loop;

      return Abs_Step;

   end Compute_Absolute_Step;

   --  -------------------------------------------------------------------------
   --  L567
   function Dense_Difference (W_Fun                    : Deriv_Fun_Access;
                              X0                       : Real_Float_Vector; F0 : Real_Float_Vector;
                              H, Use_One_Sided         : Real_Float_Vector;
                              Method                   : FD_Methods) return Real_Float_Matrix is
      use Real_Float_Arrays;
      Routine_Name  : constant String := "Num_Diff.Dense_Difference ";
      H_Vecs        : Real_Float_Matrix := Unit_Matrix (H'Length);
      J_T           : Real_Float_Matrix (F0'Range, X0'Range) :=
                        (others => (others => 0.0));
      X             : Real_Float_Vector (X0'Range);
      X1            : Real_Float_Vector (X0'Range);
      X2            : Real_Float_Vector (X0'Range);
      dX            : Float;
      dF            : Real_Float_Vector (X0'Range);
   begin
      for index in H'Range loop
         H_Vecs (index, index) := H (index);
      end loop;

      for index in H'Range loop
         case Method is
            when FD_2_Point =>
               X (index) := X0 (index) + H_Vecs (index, index);
               dX := X (index) - X0 (index);
               dF := W_Fun (X) - F0;

            when FD_3_Point =>
               if Use_One_Sided (index) /= 0.0 then
                  X1 (index) := X0 (index) + H_Vecs (index, index);
                  X2 (index) := X0 (index) + 2.0 * H_Vecs (index, index);
                  dX := X2 (index) - X0 (index);
                  dF := -3.0 * F0 + 4.0 * W_Fun (X1) - W_Fun (X2);
               else
                  X1 (index) := X0 (index) - H_Vecs (index, index);
                  X2 (index) := X0 (index) + H_Vecs (index, index);
                  dX := X2 (index) - X1 (index);
                  dF := W_Fun (X2) - W_Fun (X1);
               end if;

            when FD_CS =>
               Assert (False, Routine_Name &
                         "CS not implemented, uses imaginay values");
            when others => null;
         end case;

         for col in J_T'Range loop
            J_T (index, col) := dF (index, col) / dX;
         end loop;

      end loop;

      return Transpose (J_T);

   end Dense_Difference;

   --  -------------------------------------------------------------------------

   function EPS_For_Method (Method : FD_Methods) return Float is
      use Maths.Float_Math_Functions;
      Value : Float := EPS;
   begin
      case Method is
         when FD_2_Point | FD_CS =>
            Value := Sqrt (EPS);
         when FD_3_Point =>
            Value := EPS ** (1 / 3);
         when others => null;
      end case;

      return Value;

   end EPS_For_Method;

   --  -------------------------------------------------------------------------

   function Fun_Wrapped (Fun : Deriv_Fun_Access; X : Real_Float_Vector)
                          return Real_Float_Vector is
   begin
      return Fun (X);

   end Fun_Wrapped;

   --  -------------------------------------------------------------------------

   function Inf_Bounds (Bounds : Constraints.Bounds_List) return Boolean is
      Result : Boolean := True;
   begin
      for index in  Bounds.First_Index .. Bounds.Last_Index loop
         Result := Result and Bounds (index).Lower = Float'Last and
           Bounds.Element (index).Upper = Float'Last;
      end loop;

      return Result;

   end Inf_Bounds;

   --  -------------------------------------------------------------------------

   --     function Linear_Operator_Difference
   --       (Fun    : Deriv_Fun_Access; X0, F0 : Real_Float_Vector; H : Real_Float_List;
   --        Method : FD_Methods) return Real_Float_Vector is
   --        M      : constant Positive := F0'Length;
   --        N      : constant Positive := X0'Length;
   --        Result : Real_Float_Vector := X0;
   --     begin
   --
   --        return Mat_Vec (X0, F0, H, Method);
   --
   --     end Linear_Operator_Difference;

   --  -------------------------------------------------------------------------

   function Mat_Vec (Fun : Deriv_Fun_Access; X0 : Real_Float_Vector;
                     F0  : Real_Float_Vector;
                     H   : Real_Float_Vector; Method : FD_Methods)
                      return Real_Float_Matrix is
      use Maths.Float_Math_Functions;
      use Real_Float_Arrays;
      Routine_Name : constant String := "Num_Diff.Mat_Vec ";
      M            : constant Float := Float (F0'Length);
      N            : constant Float := Float (X0'Length);
      P            : constant Real_Float_Vector (1 .. 2) := (M, N);
      Norm_P       : constant Float := Sqrt (M ** 2 + N ** 2);
      dx           : Real_Float_Vector (H'Range);
      dx_P         : Real_Float_Vector (H'Range);
      df           : Real_Float_Matrix (F0'Range, 1 .. 1);
      X1           : Real_Float_Vector (F0'Range);
      X2           : Real_Float_Vector (F0'Range);
      Result       : Real_Float_Matrix (F0'Range, 1 .. 1) :=
                       (others => (others => 0.0));
   begin
      if F0 /= Result then
         for index in dx'Range loop
            dx (index) := H (index) / Norm_P;
            dx_P (index) := dx (index) * P (1) + dx (index) * P (2);
         end loop;

         case Method is
            when FD_2_Point =>
               X1 := X0 + dx_P;
               df := Fun (X1) - F0;
               Result := df / dx;
            when FD_3_Point =>
               dx := 2.0 * dx;
               X1 := X0 - dx_P;
               X2 := X0 + dx_P;
               df := Fun (X2) - Fun (X1);
               Result := df / dx;
            when FD_CS =>
               X1 := X0 + dx_P;
               Assert (False, Routine_Name &
                         "CS incomplete, uses complex values");
            when others => null;
         end case;
      end if;
      --  Use something like?
      --  X1 := Solve (F0, X0);
      return Result;

   end Mat_Vec;

   --  -------------------------------------------------------------------------

   function Prepare_Bounds (Bounds : Constraints.Bounds_List;
                            X0     : Real_Float_Vector)
                             return Constraints.Bounds_List is
      Result : Constraints.Bounds_List;
   begin
      if Bounds.Is_Empty then
         Result.Set_Length (X0'Length);
      else
         Result := Bounds;
      end if;

      return Result;

   end Prepare_Bounds;

   --  -------------------------------------------------------------------------

   function Relative_Step (Method : FD_Methods) return Float is
      use Maths.Float_Math_Functions;
   begin
      if Method = FD_3_Point then
         return EPS ** (1 / 3);
      else
         return Sqrt (EPS);
      end if;

   end Relative_Step;

   --  -------------------------------------------------------------------------

end Num_Diff;
