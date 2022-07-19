
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
   type Natural_Array is array (Integer range <>) of Natural;
   procedure Integer_Array_Sort is new Ada.Containers.Generic_Array_Sort
     (Integer, Integer, Integer_Array);

   type Multi_Value_Array is array (Integer range <>, Integer range <>)
     of Integer;
   type Boolean_Matrix is array (Integer range <>, Integer range <>) of Boolean;
   type Integer_Matrix is array (Integer range <>, Integer range <>) of Integer;

   package Integer_Array_Package is new
     Ada.Containers.Indefinite_Vectors (Positive, Integer_Array);
   subtype Integer_Array_List is Integer_Array_Package.Vector;

   package Real_Float_Arrays is new Generic_Real_Arrays (Float);
   subtype Real_Float_Matrix is Real_Float_Arrays.Real_Matrix;
   subtype Real_Float_Vector is Real_Float_Arrays.Real_Vector;

   package Real_Float_Package is new
     Ada.Containers.Vectors (Positive, Float);
   subtype Real_Float_List is Real_Float_Package.Vector;

   use Real_Float_Arrays;
   package Real_Matrix_List_Package is new Ada.Containers.Indefinite_Vectors
     (Positive, Real_Float_Matrix);
   subtype Real_Matrix_List is Real_Matrix_List_Package.Vector;

   package Real_Vector_List_Package is new Ada.Containers.Indefinite_Vectors
     (Positive, Real_Float_Vector);
   subtype Real_Vector_List is Real_Vector_List_Package.Vector;

   function ">=" (L : Real_Float_Vector; R : Float) return Boolean_Array;
   function ">=" (L, R : Real_Float_Vector) return Boolean_Array;
   function ">=" (L : Real_Float_Vector; R : Float) return Real_Float_Vector;
   function ">=" (L : Real_Float_Vector; R : Float) return Float_Array;
   pragma Inline (">=");
   function "<=" (L, R : Real_Float_Vector) return Boolean_Array;
   pragma Inline ("<=");
   function "<" (L, R : Real_Float_Vector) return Boolean_Array;
   pragma Inline ("<");
   function "*" (L, R : Float_Array) return Float_Array;
   function "*" (L : Float; R : Float_Array) return Float_Array;
   pragma Inline ("*");
   function "/" (L, R : Real_Float_Matrix) return Real_Float_Matrix;
   function "/" (L : Real_Float_Matrix; R : Real_Float_Vector)
                 return Real_Float_Matrix;
   function "/" (L, R : Real_Float_Vector) return Real_Float_Vector;
   pragma Inline ("/");
   function "+" (L : Real_Float_Matrix; R : Real_Float_Vector)
                 return Real_Float_Matrix;
   function "+" (L : Float; R : Real_Float_Vector) return Real_Float_Vector;
   pragma Inline ("+");
   function "-" (L, R : Integer_Matrix) return Integer_Matrix;
   function "-" (L : Real_Float_Matrix; R : Boolean_Matrix)
                 return Real_Float_Matrix;
   function "-" (L : Real_Float_Matrix; R : Float) return Real_Float_Matrix;
   function "-" (L : Real_Float_Vector; R : Float) return Real_Float_Vector;
   function "-" (L : Real_Float_Matrix; R : Real_Float_Vector)
                 return Real_Float_Matrix;
   pragma Inline ("-");
   procedure Check_Lengths (Routine_Name : String; L, R : Boolean_Matrix);
   procedure Check_Lengths (Routine_Name : String; L, R : Integer_Matrix);
   procedure Check_Lengths (Routine_Name : String; L, R : Real_Float_Matrix);
   pragma Inline (Check_Lengths);
   function Cumulative_Sum (A : Float_Array) return Float_Array;
   function Dot (L, R : Real_Float_List) return Float;
   pragma Inline (Dot);
   function Exp (M : Real_Float_Matrix) return Real_Float_Matrix;
   function Exp (V : Real_Float_Vector) return Real_Float_Vector;
   pragma Inline (Exp);
   function Flatten (M : Integer_Matrix) return Integer_Array;
   function Flatten (M : Real_Float_Matrix) return Real_Float_Vector;
   function Max_Vec (L : Float; R : Real_Float_Vector) return Real_Float_Vector;
   function Multiply_Elements (L, R : Real_Float_Matrix)
                               return Real_Float_Matrix;
   pragma Inline (Multiply_Elements);
   function Norm (M : Real_Float_List) return Float;
   function Norm (M : Real_Float_Vector) return Float;
   function Normalize (M : Float_Array) return Float_Array;
   function Normalize_Rows (M : Real_Float_Matrix) return Real_Float_Matrix;
   function "not" (M : Boolean_Matrix) return Boolean_Matrix;
   pragma Inline ("not");
   function Outer (L, R : Real_Float_Vector) return Real_Float_Vector;
   pragma Inline (Outer);
   function Sum (Data : Real_Float_Matrix) return Real_Float_Vector;
   pragma Inline (Sum);
   function Max (Data : Real_Float_Matrix) return Real_Float_Vector;
   function Max (L, R : Real_Float_Vector) return Real_Float_Vector;
   pragma Inline (Max);
   function Min (L, R : Real_Float_Vector) return Real_Float_Vector;
   pragma Inline (Min);
   function To_Boolean_Array (List : NL_Types.Boolean_List)
                              return Boolean_Array;
   function To_Boolean_Matrix (IM : Integer_Matrix) return Boolean_Matrix;
   function To_Integer_Array (List : NL_Types.Integer_List)
                              return Integer_Array;
   function To_Integer_Matrix (List : NL_Types.Integer_List_2D)
                               return Integer_Matrix;
   function To_Natural_Array (List : NL_Types.Natural_List)
                              return Natural_Array;
   function To_Real_Float_Matrix (List : NL_Types.Float_List_2D)
                                  return Real_Float_Matrix;
   function To_Real_Float_Matrix (BM : Boolean_Matrix)
                                  return Real_Float_Matrix;
   function To_Real_Float_Matrix (IM : Integer_Matrix)
                                  return Real_Float_Matrix;
   function To_Real_Float_Vector (List : Real_Float_List)
                                  return Real_Float_Vector;
   function Transpose (Values : Boolean_Matrix) return Boolean_Matrix;
   pragma Inline (Transpose);
   function Transpose (Values : Integer_Matrix) return Integer_Matrix;
   pragma Inline (Transpose);
   function Unit_Float_Matrix (Num_Rows : Positive) return Real_Float_Matrix;
   function Unit_Integer_Matrix (Num_Rows : Positive) return Integer_Matrix;
   function Zero_Array (Num_Rows : Positive) return Real_Float_Vector;
   function Zero_Matrix (Num_Rows, Num_Cols : Positive)
                         return Real_Float_Matrix;

end NL_Arrays_And_Matrices;
