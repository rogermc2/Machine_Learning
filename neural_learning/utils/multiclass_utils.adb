
with Ada.Containers.Ordered_Maps;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package body Multiclass_Utils is

   pragma Warnings (Off);

   package Label_Package is new
     Ada.Containers.Ordered_Maps (Unique_Label, Unbounded_String);

   Unique_Labels_Map : Label_Package.Map;

   function Type_Of_Target (Y : Integer_Matrix) return Y_Type is
   begin
      return Y_Continuous;
   end Type_Of_Target;

   --  -------------------------------------------------------------------------

   function Unique_Labels (Y : Integer_Matrix) return Y_Type is
   begin
      return Y_Continuous;
   end Unique_Labels;

   --  -------------------------------------------------------------------------

begin
   Unique_Labels_Map.Include (Unique_Binary,
                              To_Unbounded_String ("Unique_Multiclass"));
   Unique_Labels_Map.Include (Unique_Multiclass,
                              To_Unbounded_String ("Unique_Multiclass"));
   Unique_Labels_Map.Include (Unique_Mutilabel_Indicator,
                              To_Unbounded_String ("Unique_Indicator"));

end Multiclass_Utils;
