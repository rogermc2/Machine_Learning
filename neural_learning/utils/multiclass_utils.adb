
--  with Ada.Containers.Ordered_Maps;
--  with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with Encode_Utils;

package body Multiclass_Utils is

   pragma Warnings (Off);

--     package Label_Package is new
--       Ada.Containers.Ordered_Maps (Label_Type, Unbounded_String);

--     Unique_Labels_Map : Label_Package.Map;

   function Type_Of_Target (Y : Integer_Matrix) return Y_Type is
   begin
      return Y_Continuous;
   end Type_Of_Target;

   --  -------------------------------------------------------------------------
   --  unique_labels L101
--     function Unique_Multiclass_Set (Y : Integer_Matrix)
--                                     return Encode_Utils.Int_Sets.Set is
--     begin
--        --  L22 _unique_multiclass, return np.unique
--        return Encode_Utils.Unique (Y);
--
--     end Unique_Multiclass_Set;

   --  -------------------------------------------------------------------------
   --  L42 unique_labels
   function Unique_Labels (Y : Integer_Matrix) return NL_Types.Integer_List is
--        --  L84
--        Label_Kind   : Label_Type := Unique_Binary;
--        Label_Set    : Encode_Utils.Int_Sets.Set;
   begin
      --  L101
--        case Label_Kind is
--           --  L35
--           when Unique_Binary | Unique_Multiclass =>
--              --  L22
--              Label_Set := Unique_Multiclass_Set (Y);
--              --  L29
--           when Unique_Mutilabel_Indicator => Null;
--              Labels := Unique_Indicator (Y);
--        end case;
      --  L111
      return Encode_Utils.Unique (Y);

   end Unique_Labels;

   --  -------------------------------------------------------------------------
   --  L101
--     function Unique_Label_Set (Y : Integer_Matrix)
--                                return Encode_Utils.Int_Sets.Set is
--        Labels       : Encode_Utils.Int_Sets.Set;
--        --  L84
--        Label_Kind   : Label_Type := Unique_Binary;
--     begin
--        case Label_Kind is
--           --  L35
--           when Unique_Binary | Unique_Multiclass =>
--              --  L22
--              Labels := Unique_Multiclass_Set (Y);
--              --  L29
--           when Unique_Mutilabel_Indicator => Null;
--  --              Labels := Unique_Indicator (Y);
--        end case;
--
--        return Labels;
--
--     end Unique_Label_Set;

   --  -------------------------------------------------------------------------

end Multiclass_Utils;
