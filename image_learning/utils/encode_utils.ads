
with Ada.Containers.Ordered_Sets;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with IL_Types; use IL_Types;

package Encode_Utils is

   package Int_Sets is new Ada.Containers.Ordered_Sets (Integer);
   package UB_String_Sets is new
     Ada.Containers.Ordered_Sets (Unbounded_String);
   Encode_Error : exception;

   function Encode (Values : Integer_List)
                     return Integer_List;
   function Encode (Values        : Integer_List;
                    Uniques       : Integer_List;
                    Check_Unknown : Boolean := True)
                     return Natural_List;
   function Map_To_Integer (Values  : Integer_List; Uniques : Integer_List)
                             return Natural_List;
   function Unique (Values : Natural_List) return Natural_List;
   function Unique (Values : Integer_List)
                     return Integer_List;
   function Unique (Values  : Integer_List;
                    Inverse : out Natural_List)
                     return Integer_List;

end Encode_Utils;
