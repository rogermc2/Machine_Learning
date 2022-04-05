
with NL_Types; use NL_Types;

package NL_Arrays_And_Matrices is

   type Float_Array is array (Integer range <>) of Float;
   type Integer_Array is array (Integer range <>) of Integer;

   type Float_Matrix is array (Integer range <>, Integer range <>) of Float;
   type Integer_Matrix is array (Integer range <>, Integer range <>) of Integer;

   subtype Safe_Float is Float range Float'Range;

   function To_Integer_Array (List : Integer_List) return Integer_Array;
   function To_Float_Array (List : Float_List) return Float_Array;
   function To_Float_Matrix (List : Float_List_2D) return Float_Matrix;

end NL_Arrays_And_Matrices;
