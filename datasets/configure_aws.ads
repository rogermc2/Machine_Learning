
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Fixed;
with Ada.Containers.Hashed_Maps;
with Ada.Strings.Unbounded.Hash;
with Ada.Strings.Fixed.Hash;

package Configure_AWS is
   use Ada.Strings;

   function Hashed (X : Unbounded_String) return Ada.Containers.Hash_Type
                    renames Ada.Strings.Unbounded.Hash;

   package Config_Maps is new Ada.Containers.Hashed_Maps (
      Key_Type        => Unbounded_String,
      Element_Type    => Unbounded_String,
      Hash            => Hashed,
      Equivalent_Keys => "=");

   procedure Add_Param (Cfg : in out Config_Maps.Map; Arg : String);

end Configure_AWS;
