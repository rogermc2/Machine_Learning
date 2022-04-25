
with Ada.Containers.Ordered_Sets;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with NL_Types;
with NL_Arrays_And_Matrices; use NL_Arrays_And_Matrices;

package Encode_Utils is

   package Int_Sets is new Ada.Containers.Ordered_Sets (Integer);
   package UB_String_Sets is new
     Ada.Containers.Ordered_Sets (Unbounded_String);
   Encode_Error : exception;

   function Encode (Values : Integer_Array) return Integer_Array;
   function Encode (Values : Integer_Array; Uniques : Integer_Array;
                    Check_Unknown : Boolean := True) return Natural_Array;
   function Map_To_Integer (Values  : Integer_Array;
                            Uniques : Integer_Array) return Natural_Array;
   function Unique (Values : Natural_Array) return Natural_Array;
   function Unique (Values : Integer_Array) return Integer_Array;
   function Unique (Values : Integer_Array; Inverse : out Natural_Array)
                    return Integer_Array;
   function Unique (Values : Integer_Matrix) return Integer_Array;
   function Unique (Values : Integer_Matrix) return NL_Types.Integer_List;
   function Unique (Values : Integer_Matrix) return Int_Sets.Set;

end Encode_Utils;
