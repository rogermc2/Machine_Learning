--  Based on scipy/optimise/_constraints.py

with Ada.Containers.Vectors;

with NL_Types;

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

end Constraints;
