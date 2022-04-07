
with NL_Types; use NL_Types;

package NL_Arrays_And_Matrices is

   type Float_Array is array (Integer range <>) of Float;
   type Integer_Array is array (Integer range <>) of Integer;

   type Float_Matrix is array (Integer range <>, Integer range <>) of Float;
   type Integer_Matrix is array (Integer range <>, Integer range <>) of Integer;

   subtype Safe_Float is Float range Float'Range;

   function "*" (L, R : Float_Matrix) return Float_Matrix;
   pragma Inline ("*");
   function "**" (L : Float_Matrix; P : Integer) return Float_Matrix;
   pragma Inline ("**");
   function "/" (L, R : Float_Matrix) return Float_Matrix;
   function "/" (L : Float_Matrix; R : Float) return Float_Matrix;
   pragma Inline ("/");
   function "-" (L, R : Float_Matrix) return Float_Matrix;
   pragma Inline ("-");
   function To_Float_Matrix (IM : Integer_Matrix) return Float_Matrix;
   pragma Inline (To_Float_Matrix);
   function To_Float_Array (List : Float_List) return Float_Array;
   function To_Float_Matrix (List : Float_List_2D) return Float_Matrix;
   function To_Integer_Array (List : Integer_List) return Integer_Array;

end NL_Arrays_And_Matrices;
