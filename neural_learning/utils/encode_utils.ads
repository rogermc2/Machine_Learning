
with Ada.Containers.Ordered_Sets;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with NL_Types;
with NL_Arrays_And_Matrices; use NL_Arrays_And_Matrices;

package Encode_Utils is

   package Bool_Sets is new Ada.Containers.Ordered_Sets (Boolean);
   package Float_Sets is new Ada.Containers.Ordered_Sets (Float);
   package Int_Sets is new Ada.Containers.Ordered_Sets (Integer);
   package UB_String_Sets is new
     Ada.Containers.Ordered_Sets (Unbounded_String);

   Encode_Error : exception;

   function Encode (Values : Integer_Array) return Integer_Array;
   function Encode (Values : Integer_Array; Uniques : Integer_Array;
                    Check_Unknown : Boolean := True) return Natural_Array;
   function Map_To_Integer (Values  : Integer_Array;
                            Uniques : Integer_Array) return Natural_Array;
   function Unique (Values : Binary_Matrix) return NL_Types.Integer_List;
   function Unique (Values : Natural_Array) return Natural_Array;
   function Unique (Values : Integer_Array) return Integer_Array;
   function Unique (Values : Boolean_Array) return Boolean_Array;
   function Unique (Values : Integer_Array) return NL_Types.Integer_List;
   function Unique (List : Integer_Array_List) return NL_Types.Integer_List;
   function Unique (Values : NL_Types.Integer_List)
                    return NL_Types.Integer_List;
   function Unique (Values : Integer_Array; Inverse : out Natural_Array)
                    return Integer_Array;
   function Unique (Values : Integer_Matrix) return Integer_Array;
   function Unique (Values : Integer_Matrix) return NL_Types.Integer_List;
   function Unique (Values : Integer_Matrix) return Int_Sets.Set;
   function Unique (Values : NL_Types.Array_Of_Integer_Lists)
                    return NL_Types.Integer_List;
   function Unique (Values : Real_Float_Matrix) return NL_Types.Float_List;
   function Unique (Values : Unbounded_String_Array)
                    return NL_Types.Unbounded_List;
   function Unique (Values : Unbounded_String_Matrix)
                    return NL_Types.Unbounded_List;

end Encode_Utils;
