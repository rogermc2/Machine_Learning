--  Based on scipy/optimize/_numdiff.py

with Ada.Assertions; use Ada.Assertions;
with Ada.Containers;

package body Num_Diff is

   function Inf_Bounds (Bounds : Constraints.Bounds_List) return Boolean;
   function Prepare_Bounds (Bounds : Constraints.Bounds_List;
                            X0     : Real_Float_Vector)
                            return Constraints.Bounds_List;

   --  -------------------------------------------------------------------------

   function Approx_Derivative
     (Fun                : access function (X : Fortran_DP_Array) return Float;
      X0                 : Real_Float_Vector;
      Method             : FD_Methods := FD_None;
      Rel_Step           : NL_Types.Integer_List :=
        NL_Types.Integer_Package.Empty_Vector;
      Abs_Step           : NL_Types.Integer_List :=
        NL_Types.Integer_Package.Empty_Vector;
      F0                 : NL_Types.Float_List :=
        NL_Types.Float_Package.Empty_Vector;
      Bounds             : Constraints.Bounds_List :=
        Constraints.Array_Bounds_Package.Empty_Vector;
      As_Linear_Operator : Boolean := False) return Real_Float_Vector is

      use  Ada.Containers;
      Routine_Name : constant String := "Num_Diff.Approx_Derivative ";
      Loc_Bounds   : constant Constraints.Bounds_List :=
                       Prepare_Bounds (Bounds, X0);
      Result       : Real_Float_Vector (X0'Range);
   begin
      Assert (Loc_Bounds.Length = X0'Length, Routine_Name &
                "Bounds and X0 lengths unequal.");
      if As_Linear_Operator then
          Assert (Inf_Bounds (Bounds), Routine_Name &
                "Bounds not supported for Linear_Operator.");
      end if;

      return Result;

   end Approx_Derivative;

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

end Num_Diff;
