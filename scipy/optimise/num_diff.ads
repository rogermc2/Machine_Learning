--  Based on scipy/optimize/_numdiff.py

with Constraints;
--  with Lbfgsb_F_Interface; use Lbfgsb_F_Interface;
with NL_Arrays_And_Matrices; use NL_Arrays_And_Matrices;
--  with NL_Types;

package Num_Diff is

   type FD_Methods is (FD_None, FD_2_Point, FD_3_Point, FD_CS);
   type Fun_Access is access function (X : Real_Float_Vector)
                                       return Real_Float_Vector;
   type Dimensions is record
      Rows : Positive := 1;
      Cols : Positive := 1;
   end record;

   type Linear_Operator is record
      Dims    : Dimensions;
      Mat_Vec : Fun_Access;  --  returns A * v.
   end record;

   function Approx_Derivative
     (Fun                : Fun_Access;
      X0                 : Real_Float_Vector;
      Method             : FD_Methods := FD_None;
      Rel_Step           : Real_Float_List := Real_Float_Package.Empty_Vector;
      Abs_Step           : Real_Float_Vector;
      F0                 : Real_Float_Vector;
      Bounds             : Constraints.Bounds_List :=
        Constraints.Array_Bounds_Package.Empty_Vector;
      As_Linear_Operator : Boolean := False) return Real_Float_Matrix;

end Num_Diff;
