--  Based on scipy/optimise/_constraints.py

with Ada.Containers.Vectors;

with NL_Types;
with ML_Arrays_And_Matrices; use ML_Arrays_And_Matrices;

package Opt_Constraints is

   type Array_Bounds is record
      Lower : Float := Float'Safe_First;
      Upper : Float := Float'Safe_Last;
   end record;

   Default_Bounds : constant Array_Bounds :=
                       (Float'Safe_First, Float'Safe_Last);

   package Array_Bounds_Package is new Ada.Containers.Vectors
     (Positive, Array_Bounds);
   subtype Bounds_List is Array_Bounds_Package.Vector;

   procedure Get_Bounds (Bounds       : Bounds_List;
                         Lower, Upper : out NL_Types.Float_List);
   function Get_Lower (Bounds : Bounds_List) return Real_Float_Vector;
   function Get_Upper (Bounds : Bounds_List) return Real_Float_Vector;

end Opt_Constraints;
