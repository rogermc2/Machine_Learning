
with Ada.Containers.Generic_Array_Sort;
with Ada.Containers.Indefinite_Vectors;
with Ada.Containers.Vectors;
with Ada.Numerics.Generic_Real_Arrays;

with NL_Types;

package NL_Arrays_And_Matrices is
    use Ada.Numerics;

    subtype Safe_Float is Float range Float'Range;

    type Boolean_Array is array (Integer range <>) of Boolean;
    type Float_Array is array (Integer range <>) of Float;
    type Integer_Array is array (Integer range <>) of Integer;
    type Long_Float_Array is array (Integer range <>) of Long_Float;
    type Natural_Array is array (Integer range <>) of Natural;
    procedure Integer_Array_Sort is new Ada.Containers.Generic_Array_Sort
      (Integer, Integer, Integer_Array);

    type Multi_Value_Array is array (Integer range <>, Integer range <>)
      of Integer;
    type Boolean_Matrix is array (Integer range <>, Integer range <>) of Boolean;
    type Float_Matrix is array (Integer range <>, Integer range <>) of Float;
    type Integer_Matrix is array (Integer range <>, Integer range <>) of Integer;

    package Matrix_List_Package is new Ada.Containers.Indefinite_Vectors
      (Positive, Float_Matrix);
    subtype Matrix_List is Matrix_List_Package.Vector;

    package Long_Float_Arrays is new Generic_Real_Arrays (Long_Float);
    type Long_Float_Matrix is new Long_Float_Arrays.Real_Matrix;
    type Long_Float_Vector is new Long_Float_Arrays.Real_Vector;

   package Long_Float_Package is new
      Ada.Containers.Vectors (Positive, Long_Float);
   subtype Long_Float_List is Long_Float_Package.Vector;

    package Long_Matrix_List_Package is new Ada.Containers.Indefinite_Vectors
      (Positive, Long_Float_Matrix);
    subtype Long_Matrix_List is Long_Matrix_List_Package.Vector;

    function "*" (L, R : Float_Matrix) return Float_Matrix;
    function "*" (L : Float; R : Float_Matrix) return Float_Matrix;
    pragma Inline ("*");
    function "**" (L : Float_Array; P : Integer) return Float_Array;
    function "**" (L : Float_Matrix; P : Integer) return Float_Matrix;
    pragma Inline ("**");
    function "/" (L, R : Float_Matrix) return Float_Matrix;
    function "/" (L : Float_Matrix; R : Float) return Float_Matrix;
    function "/" (L : Float_Matrix; R : Float_Array) return Float_Matrix;
    function "/" (L : Long_Float_Matrix; R : Long_Float_Array)
                  return Long_Float_Matrix;
    pragma Inline ("/");
    function "+" (L, R : Float_Array) return Float_Array;
    function "+" (L, R : Float_Matrix) return Float_Matrix;
    function "+" (L : Float_Matrix; R : Float_Array) return Float_Matrix;
--      function "+" (L, R : Long_Float_Matrix) return Long_Float_Matrix;
    pragma Inline ("+");
    function "-" (M : Float_Matrix) return Float_Matrix;
    function "-" (L, R : Float_Matrix) return Float_Matrix;
    function "-" (L, R : Integer_Matrix) return Integer_Matrix;
    function "-" (L : Float_Matrix; R : Float_Array) return Float_Matrix;
    function "-" (L : Float; R : Float_Matrix) return Float_Matrix;
    function "-" (L : Long_Float_Matrix; R : Long_Float_Array)
                  return Long_Float_Matrix;
    pragma Inline ("-");
    procedure Check_Lengths (Routine_Name : String; L, R : Float_Matrix);
    function Diff_Max (Data : Float_Matrix; Axis : Positive) return Float_Matrix;
    function Dot (L, R : Float_Matrix) return Float_Matrix;
    function Dot (L : Float_Matrix; R : Float_Array)
                 return Float_Array;
    function Dot (L, R : Long_Float_List) return Long_Float;
    pragma Inline (Dot);
    function Exp (M : Float_Matrix) return Float_Matrix;
    function Exp (M : Long_Float_Matrix) return Long_Float_Matrix;
    pragma Inline (Exp);
    function Flatten (M : Float_Matrix) return Float_Array;
    function Flatten (M : Integer_Matrix) return Integer_Array;
    function Get_Row (M : Float_Matrix; R : Integer) return Float_Array;
    pragma Inline (Get_Row);
    function "not" (M : Boolean_Matrix) return Boolean_Matrix;
    pragma Inline ("not");
    function Sum (Data : Float_Matrix) return Float_Array;
    function Sum (Data : Long_Float_Matrix) return Long_Float_Array;
    pragma Inline (Sum);
    function Max (Data : Float_Matrix) return Float_Array;
    function Max (Data : Long_Float_Matrix) return Long_Float_Array;
    pragma Inline (Max);
    function To_Boolean_Array (List : NL_Types.Boolean_List)
                              return Boolean_Array;
    function To_Float_Matrix (IM : Integer_Matrix) return Float_Matrix;
    function To_Float_Array (List : NL_Types.Float_List) return Float_Array;
    pragma Inline (To_Float_Array);
    function To_Float_Matrix (List : NL_Types.Float_List_2D)
                              return Float_Matrix;
    function To_Integer_Array (List : NL_Types.Integer_List)
                              return Integer_Array;
    function To_Long_Float_Matrix (IM : Integer_Matrix)
                                   return Long_Float_Matrix;
    function To_Natural_Array (List : NL_Types.Natural_List)
                              return Natural_Array;
    function Transpose (Values : Boolean_Matrix) return Boolean_Matrix;
    function Transpose (Values : Float_Matrix) return Float_Matrix;
    function Transpose (Values : Integer_Matrix) return Integer_Matrix;
    pragma Inline (Transpose);

end NL_Arrays_And_Matrices;
