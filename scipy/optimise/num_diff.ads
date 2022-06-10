--  Based on scipy/optimize/_numdiff.py

with Constraints;
with Lbfgsb_F_Interface; use Lbfgsb_F_Interface;
with NL_Arrays_And_Matrices; use NL_Arrays_And_Matrices;
with NL_Types;

package Num_Diff is

   type FD_Methods is (FD_None, FD_2_Point, FD_3_Point, FD_CS);

   function Approx_Derivative
     (Fun                : access function (X : Fortran_DP_Array)
      return Real_Float_Vector;
      X0                 : Real_Float_Vector;
      Method             : FD_Methods := FD_None;
      Rel_Step           : Real_Float_List := Real_Float_Package.Empty_Vector;
      Abs_Step           : NL_Types.Integer_List :=
        NL_Types.Integer_Package.Empty_Vector;
      F0                 : Real_Float_Vector;
      Bounds             : Constraints.Bounds_List :=
        Constraints.Array_Bounds_Package.Empty_Vector;
      As_Linear_Operator : Boolean := False) return Real_Float_Vector;

end Num_Diff;
