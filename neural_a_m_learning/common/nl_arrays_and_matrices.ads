
with Ada.Containers.Indefinite_Vectors;

with NL_Types;

package NL_Arrays_And_Matrices is

   subtype Safe_Float is Float range Float'Range;

   type Float_Array is array (Integer range <>) of Float;
   type Integer_Array is array (Integer range <>) of Integer;
   type Natural_Array is array (Integer range <>) of Natural;

   type Float_Matrix is array (Integer range <>, Integer range <>) of Float;
   type Integer_Matrix is array (Integer range <>, Integer range <>) of Integer;

   package Matrix_List_Package is new Ada.Containers.Indefinite_Vectors
     (Positive, Float_Matrix);
   subtype Matrix_List is Matrix_List_Package.Vector;

   function "*" (L, R : Float_Matrix) return Float_Matrix;
   function "*" (L : Float; R : Float_Matrix) return Float_Matrix;
   pragma Inline ("*");
   function "**" (L : Float_Array; P : Integer) return Float_Array;
   function "**" (L : Float_Matrix; P : Integer) return Float_Matrix;
   pragma Inline ("**");
   function "/" (L, R : Float_Matrix) return Float_Matrix;
   function "/" (L : Float_Matrix; R : Float) return Float_Matrix;
   pragma Inline ("/");
   function "+" (L, R : Float_Array) return Float_Array;
   function "+" (L, R : Float_Matrix) return Float_Matrix;
   pragma Inline ("+");
   function "-" (L, R : Float_Matrix) return Float_Matrix;
   pragma Inline ("-");
   function Dot (L, R : Float_Matrix) return Float_Matrix;
   pragma Inline (Dot);
   function To_Float_Matrix (IM : Integer_Matrix) return Float_Matrix;
   pragma Inline (To_Float_Matrix);
   function To_Float_Array (List : NL_Types.Float_List) return Float_Array;
   function To_Float_Matrix (List : NL_Types.Float_List_2D) return Float_Matrix;
   function To_Integer_Array (List : NL_Types.Integer_List) return Integer_Array;
   function Transpose (Values : Float_Matrix) return Float_Matrix;
   pragma Inline (Transpose);

end NL_Arrays_And_Matrices;
