--  Based on scipy/optimise/_constraints.py

with Ada.Containers.Vectors;

with NL_Types;
with NL_Arrays_And_Matrices; use NL_Arrays_And_Matrices;

package Constraints is

   type Array_Bounds is record
      Lower : Float := Float'Safe_First;
      Upper : Float := Float'Safe_Last;
   end record;

   package Array_Bounds_Package is new Ada.Containers.Vectors
     (Positive, Array_Bounds);
   subtype Bounds_List is Array_Bounds_Package.Vector;

   procedure Get_Bounds (Bounds       : Bounds_List;
                         Lower, Upper : out NL_Types.Float_List);
   function Get_Lower (Bounds : Bounds_List) return Real_Float_Vector;
   function Get_Upper (Bounds : Bounds_List) return Real_Float_Vector;

end Constraints;
