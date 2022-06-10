--  Based on scipy/optimize/_numdiff.py

with Ada.Assertions; use Ada.Assertions;
with Ada.Containers;
with Ada.Text_IO; use Ada.Text_IO;

with Maths;

package body Num_Diff is

   EPS : constant Float := Float'Small;

   function Compute_Absolute_Step
     (Rel_Step : in out Real_Float_List; X0 : Real_Float_Vector;
      Method   : FD_Methods) return Real_Float_List;
   function Inf_Bounds (Bounds : Constraints.Bounds_List) return Boolean;
   --     function Linear_Operator_Difference
   --       (Fun    : Fun_Access; X0, F0 : Real_Float_Vector; H : Real_Float_List;
   --        Method : FD_Methods) return Real_Float_Vector;
   function Mat_Vec (Fun    : Fun_Access; X0, F0 : Real_Float_Vector;
                     H      : Real_Float_List; Method : FD_Methods)
                     return Real_Float_Vector;
   function Prepare_Bounds (Bounds : Constraints.Bounds_List;
                            X0     : Real_Float_Vector)
                            return Constraints.Bounds_List;
   function Relative_Step (Method : FD_Methods) return Float;

   --  -------------------------------------------------------------------------

   function Approx_Derivative
     (Fun                : Fun_Access; X0 : Real_Float_Vector;
      Method             : FD_Methods := FD_None;
      Rel_Step           : Real_Float_List := Real_Float_Package.Empty_Vector;
      Abs_Step           : NL_Types.Integer_List :=
        NL_Types.Integer_Package.Empty_Vector;
      F0                 : Real_Float_Vector;
      Bounds             : Constraints.Bounds_List :=
        Constraints.Array_Bounds_Package.Empty_Vector;
      As_Linear_Operator : Boolean := False) return Real_Float_Vector is
      use  Ada.Containers;
      Routine_Name  : constant String := "Num_Diff.Approx_Derivative ";
      Loc_Bounds    : constant Constraints.Bounds_List :=
                        Prepare_Bounds (Bounds, X0);
      L_Rel_Step    : Real_Float_List := Rel_Step;
      Use_One_Sided : Boolean;
      H             : Real_Float_List;
      df_dx         : Real_Float_Vector (X0'Range);

   begin
      --  L339
      Assert (Loc_Bounds.Length = X0'Length, Routine_Name &
                "Bounds and X0 lengths unequal.");
      if As_Linear_Operator then
         Assert (Inf_Bounds (Bounds), Routine_Name &
                   "Bounds not supported for Linear_Operator.");
      end if;

      --  L363
      if As_Linear_Operator then
         if L_Rel_Step.Is_Empty then
            L_Rel_Step (1) := Relative_Step (Method);
         end if;
         df_dx := Mat_Vec (Fun, X0, F0, H, Method);
         --           Result := Linear_Operator_Difference
         --             (Fun_Wrapped'Access, X0, F0, L_Rel_Step, Method);
      else
         if Abs_Step.Is_Empty then
            null;
         else
            H := Compute_Absolute_Step (L_Rel_Step, X0, Method);
         end if;

         case Method is
            when FD_2_Point => null;
            when FD_3_Point => null;
            when FD_CS => Use_One_Sided := False;
            when FD_None => null;
         end case;
      end if;

      --  L441 return LinearOperator((m, n), matvec)
      --  LinearOperator implements the matrix operations used by matvec
      return df_dx;

   end Approx_Derivative;

   --  -------------------------------------------------------------------------

   function Compute_Absolute_Step
     (Rel_Step : in out Real_Float_List; X0  : Real_Float_Vector;
      Method   : FD_Methods) return Real_Float_List is
      Result : Real_Float_List;
   begin
      if Rel_Step.Is_Empty then
         Rel_Step.Append (Relative_Step (Method));
      end if;

      return Result;

   end Compute_Absolute_Step;

   --  -------------------------------------------------------------------------

   function Fun_Wrapped (Fun : Fun_Access; X : Real_Float_Vector)
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
   --       (Fun    : Fun_Access; X0, F0 : Real_Float_Vector; H : Real_Float_List;
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

   function Mat_Vec (Fun    : Fun_Access; X0, F0 : Real_Float_Vector;
                     H      : Real_Float_List; Method : FD_Methods)
                     return Real_Float_Vector is
      use Maths.Float_Math_Functions;
      use Real_Float_Arrays;
      Routine_Name : constant String := "Num_Diff.Mat_Vec ";
      M            : constant Float := Float (F0'Length);
      N            : constant Float := Float (X0'Length);
      P            : constant Real_Float_Vector (1 .. 2) := (M, N);
      Norm_P  : constant Float := Sqrt (M ** 2 + N ** 2);
      dx      : Real_Float_Vector (1 .. Positive (H.Length));
      dx_P    : Real_Float_Vector (1 .. Positive (H.Length));
      df      : Real_Float_Vector (F0'Range);
      X1      : Real_Float_Vector (F0'Range);
      X2      : Real_Float_Vector (F0'Range);
      Result  : Real_Float_Vector (F0'Range) := (others => 0.0);
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
            when FD_None => null;
         end case;
      end if;

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
