--  Based on scipy/optimize/_numdiff.py

with Ada.Assertions; use Ada.Assertions;
with Ada.Containers;

with Maths;

package body Num_Diff is

    EPS : constant Float := Float'Small;

    function Compute_Absolute_Step (Rel_Step : in out Real_Float_List;
                                    X0 : Real_Float_Vector;
                                    Method : FD_Methods) return Float;
    function Inf_Bounds (Bounds : Constraints.Bounds_List) return Boolean;
    function Prepare_Bounds (Bounds : Constraints.Bounds_List;
                             X0     : Real_Float_Vector)
                             return Constraints.Bounds_List;
    function Relative_Step (Method : FD_Methods) return Float;

    --  -------------------------------------------------------------------------

    function Approx_Derivative
      (Fun : access function (X : Fortran_DP_Array) return Real_Float_Vector;
       X0                 : Real_Float_Vector;
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
        Relative_Step : Real_Float_List := Rel_Step;
        Use_One_Sided : Boolean;
        H             : Float;
        Result        : Real_Float_Vector (X0'Range);

        function Fun_Wrapped (X : Fortran_DP_Array) return Real_Float_Vector is
        begin
            return Fun (X);

        end Fun_Wrapped;

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
            null;
        else
            if Abs_Step.Is_Empty then
                null;
            else
                H := Compute_Absolute_Step (Relative_Step, X0, Method);
            end if;

            case Method is
            when FD_2_Point => null;
            when FD_3_Point => null;
            when FD_CS => Use_One_Sided := False;
            when FD_None => null;
            end case;
        end if;

        return Result;

    end Approx_Derivative;

    --  -------------------------------------------------------------------------

    function Compute_Absolute_Step (Rel_Step : in out Real_Float_List;
                                    X0 : Real_Float_Vector;
                                    Method : FD_Methods) return Float is
        Result : Float;
    begin
        if Rel_Step.Is_Empty then
            Rel_Step.Append (Relative_Step (Method));
        end if;

        return Result;

    end Compute_Absolute_Step;

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
