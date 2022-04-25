
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

   function Unique_Labels (Y : Integer_Matrix) return Boolean_Matrix is
        Num_Classes : Positive := 10;
        Y_Bin       : Boolean_Matrix (Y'Range, 1 .. Num_Classes);
   begin
      return Y_Bin;

   end Unique_Labels;

   --  -------------------------------------------------------------------------

end Multiclass_Utils;
