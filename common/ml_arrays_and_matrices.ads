with Interfaces;

with Ada.Containers.Generic_Array_Sort;
with Ada.Containers.Indefinite_Vectors;
with Ada.Containers.Vectors;
with Ada.Numerics.Generic_Real_Arrays;
with Ada.Strings.Unbounded;

with ML_Types;
with NL_Types;

package ML_Arrays_And_Matrices is
   use Ada.Numerics;

   subtype Safe_Float is Float range Float'Range;
   subtype Binary is Integer range 0 .. 1;

   type Binary_Array is array (Integer range <>) of Binary;
   type Boolean_Array is array (Integer range <>) of Boolean;
   type Byte_Array is array (Integer range <>) of Interfaces.Unsigned_8;
   type Float_Array is array (Integer range <>) of Float;
   procedure Float_Array_Sort is new Ada.Containers.Generic_Array_Sort
     (Integer, Float, Float_Array);
   type Integer_Array is array (Natural range <>) of Integer;
   type Natural_Array is array (Integer range <>) of Natural;
   procedure Integer_Array_Sort is new Ada.Containers.Generic_Array_Sort
     (Natural, Integer, Integer_Array);

   type Unbounded_String_Array is array (Integer range <>) of
     Ada.Strings.Unbounded.Unbounded_String;
   type Unbounded_String_Matrix is
     array (Integer range <>, Integer range <>) of
     Ada.Strings.Unbounded.Unbounded_String;

   package Unbounded_String_Array_Package is new
     Ada.Containers.Indefinite_Vectors (Positive, Unbounded_String_Array);
   subtype Unbounded_String_Array_List is
     Unbounded_String_Array_Package.Vector;

   type Multi_Value_Array is array (Integer range <>, Integer range <>)
     of Integer;
   type Binary_Matrix is array (Integer range <>, Integer range <>) of Binary;
   type Boolean_Matrix is array (Integer range <>, Integer range <>) of
     Boolean;
   type Integer_Matrix is array (Integer range <>, Integer range <>) of
     Integer;

   type Unsigned_8_Array_3D is array (Integer range <>, Integer range <>,
                                      Integer range <>) of Interfaces.Unsigned_8;

   package Integer_Array_Package is new
     Ada.Containers.Indefinite_Vectors (Positive, Integer_Array);
   subtype Integer_Array_List is Integer_Array_Package.Vector;

   package Real_Float_Arrays is new Generic_Real_Arrays (Float);
   subtype Real_Float_Matrix is Real_Float_Arrays.Real_Matrix;
   subtype Real_Float_Vector is Real_Float_Arrays.Real_Vector;

   subtype String_1 is String (1 .. 1);
   subtype String_2 is String (1 .. 2);
   subtype String_3 is String (1 .. 3);

   type String1_Array is array (1 .. 1) of String_1;
   type String21_Array is array (1 .. 2) of String_1;
   type String2_Array is array (1 .. 2) of String_2;
   type String3_Array is array (1 .. 2) of String_3;

   package String1_Package is new Ada.Containers.Vectors (Positive, String_1);
   subtype String_Array is String1_Package.Vector;

   type String_Matrix is array (1 .. 2, 1 .. 2) of String_1;
   package String2_Matrix_Package is new Ada.Containers.Vectors
     (Positive, String_Matrix);
   subtype String2_Matrix_Array is String2_Matrix_Package.Vector;

   package Binary_List_Package is new Ada.Containers.Indefinite_Vectors
     (Positive, Binary_Array);
   subtype Binary_List is Binary_List_Package.Vector;

   package Binary_Matrix_List_Package is new Ada.Containers.Indefinite_Vectors
     (Positive, Binary_Matrix);
   subtype Binary_Matrix_List is Binary_Matrix_List_Package.Vector;

   package Boolean_Matrix_List_Package is new Ada.Containers.Indefinite_Vectors
     (Positive, Boolean_Matrix);
   subtype Boolean_Matrix_List is Boolean_Matrix_List_Package.Vector;

   package Real_Float_Package is new
     Ada.Containers.Vectors (Positive, Float);
   subtype Real_Float_List is Real_Float_Package.Vector;

   use Real_Float_Package;
   package Real_Float_Package_2D is new
     Ada.Containers.Vectors (Positive, Real_Float_List);
   subtype Real_Float_List_2D is Real_Float_Package_2D.Vector;

   package Integer_Matrix_List_Package is new Ada.Containers.Indefinite_Vectors
     (Positive, Integer_Matrix);
   subtype Integer_Matrix_List is Integer_Matrix_List_Package.Vector;

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
   function "<" (L, R : Integer_Array) return Boolean_Array;
   function "<" (L, R : Real_Float_Vector) return Boolean_Array;
   pragma Inline ("<");
   function "*" (L, R : Float_Array) return Float_Array;
   function "*" (L : Float; R : Float_Array) return Float_Array;
   pragma Inline ("*");
   function "**" (L : Real_Float_Matrix; R : Natural) return Real_Float_Matrix;
   function "**" (L : Real_Float_Vector; R : Natural) return Real_Float_Vector;
   pragma Inline ("**");
   function "/" (L, R : Binary_Array) return Real_Float_Vector;
   function "/" (L : Float; R : Real_Float_Matrix) return Real_Float_Matrix;
   function "/" (L : Float; R : Real_Float_Vector) return Real_Float_Vector;
   function "/" (L, R : Real_Float_Matrix) return Real_Float_Matrix;
   function "/" (L : Real_Float_Matrix; R : Real_Float_Vector)
                  return Real_Float_Matrix;
   function "/" (L, R : Real_Float_Vector) return Real_Float_Vector;
   pragma Inline ("/");
   function "+" (L : Real_Float_Matrix; R : Real_Float_Vector)
                  return Real_Float_Matrix;
   function "+" (L : Float; R : Real_Float_Vector) return Real_Float_Vector;
   function "+" (L : Float; R : Real_Float_Matrix) return Real_Float_Matrix;
   function "+" (L, R : Integer_Matrix) return Integer_Matrix;
   pragma Inline ("+");
   function "-" (L, R : Binary_Array) return Binary_Array;
   function "-" (L, R : Binary_Matrix) return Binary_Matrix;
   function "-" (M : Integer_Matrix) return Integer_Matrix;
   function "-" (L, R : Integer_Matrix) return Integer_Matrix;
   function "-" (L : Real_Float_Matrix; R : Boolean_Matrix)
                  return Real_Float_Matrix;
   function "-" (L : Real_Float_Matrix; R : Float) return Real_Float_Matrix;
   function "-" (L : Real_Float_Vector) return Real_Float_Vector;
   function "-" (L : Float; R : Real_Float_Vector) return Real_Float_Vector;
   function "-" (L : Real_Float_Vector; R : Float) return Real_Float_Vector;
   function "-" (L : Real_Float_Matrix; R : Real_Float_Vector)
                  return Real_Float_Matrix;
   pragma Inline ("-");
   function "&" (L, R : Integer_Matrix) return Integer_Matrix;
   pragma Inline ("&");
   procedure Check_Lengths (Routine_Name : String; L, R : Boolean_Matrix);
   procedure Check_Lengths (Routine_Name : String; L, R : Integer_Matrix);
   procedure Check_Lengths (Routine_Name : String;
                            L            : Binary_Matrix; R : Integer_Matrix);
   procedure Check_Lengths (Routine_Name : String; L, R : Real_Float_Matrix);
   pragma Inline (Check_Lengths);
   function Cumulative_Sum (A : Float_Array) return Float_Array;
   function Dot (L, R : Real_Float_List) return Float;
   function Dot (L : Integer_Matrix; R : Real_Float_Vector)
                 return Real_Float_Vector;
   function Dot (L : Real_Float_Vector; R : Integer_Matrix)
                 return Real_Float_Vector;
   pragma Inline (Dot);
   function Exp (M : Real_Float_Matrix) return Real_Float_Matrix;
   function Exp (V : Real_Float_Vector) return Real_Float_Vector;
   pragma Inline (Exp);
   function Flatten (M : Integer_Matrix) return Integer_Array;
   function Flatten (M : Real_Float_Matrix) return Real_Float_Vector;
   pragma Inline (Flatten);
   function Get_Row (Matrix : Binary_Matrix; Row : Integer)
                     return Binary_Array;
   function Get_Row (Matrix : Integer_Matrix; Row : Integer)
                     return Integer_Array;
   function Get_Row (Matrix : Real_Float_Matrix; Row : Integer)
                     return Real_Float_Vector;
   pragma Inline (Get_Row);
   --  Hadamard product
   function H_Product (L, R : Binary_Array) return Binary_Array;
   function H_Product (L, R : Real_Float_Vector) return Real_Float_Vector;
   function H_Product (L : Real_Float_Vector; R : Real_Float_List)
                       return Real_Float_Vector;
   function H_Product (L : Real_Float_List; R : Real_Float_Vector)
                       return Real_Float_Vector;
   function H_Product (L, R : Real_Float_List) return Real_Float_Vector;
   function H_Product (L, R : Real_Float_Matrix) return Real_Float_Matrix;
   pragma Inline (H_Product);
   function Log (V : Real_Float_Vector) return Real_Float_Vector;
   pragma Inline (Log);
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
   function Sum (Data : Integer_Matrix) return Integer;
   function Sum (Data : Real_Float_Matrix) return Float;
   function Sum (Data : Real_Float_Matrix) return Real_Float_Vector;
   function Sum (Data : Real_Float_Vector) return Float;
   pragma Inline (Sum);
   function Sum_Diagonal (Data : Integer_Matrix) return Integer;
   pragma Inline (Sum_Diagonal);
   function Sum_Each_Column (Data : Real_Float_Matrix) return Real_Float_Vector;
   pragma Inline (Sum_Each_Column);
   function Max (Data : Real_Float_Matrix) return Float;
   function Max (Data : Real_Float_Matrix) return Real_Float_Vector;
   function Max (L, R : Real_Float_Vector) return Real_Float_Vector;
   function Max (V : Real_Float_Vector) return Float;
   pragma Inline (Max);
   function Min (Data : Real_Float_Matrix) return Float;
   function Min (L, R : Real_Float_Vector) return Real_Float_Vector;
   function Min (V : Real_Float_Vector) return Float;
   pragma Inline (Min);
   function Slice (Matrix : Binary_Matrix; First, Last : Positive)
                   return Binary_Matrix;
   function Slice (Matrix : Real_Float_Matrix; First, Last : Positive)
                   return Real_Float_Matrix;
   function Slice (Matrix : Integer_Matrix; First, Last : Positive)
                   return Integer_Matrix;
   function To_Boolean_Array (List : NL_Types.Boolean_List)
                               return Boolean_Array;
   pragma Inline (To_Boolean_Array);
   function To_Boolean_Array_Of_Lists (List : NL_Types.Boolean_List_2D)
                              return NL_Types.Boolean_Array_Of_Lists;
   pragma Inline (To_Boolean_Array_Of_Lists);
   function To_Boolean_Matrix (List : NL_Types.Boolean_List_2D)
                               return Boolean_Matrix;
   function To_Boolean_Matrix (IM : Integer_Matrix) return Boolean_Matrix;
   pragma Inline (To_Boolean_Matrix);
   function To_Integer_Array (List : ML_Types.Integer_List)
                               return Integer_Array;
   pragma Inline (To_Integer_Array);
   function To_Integer_Matrix (List : ML_Types.Integer_List_2D)
                                return Integer_Matrix;
   function To_Integer_Matrix (List : ML_Types.Value_Data_Lists_2D)
                               return Integer_Matrix;
   function To_Integer_Matrix (Bin : Binary_Matrix) return Integer_Matrix;
   function To_Integer_Matrix (Bool : Boolean_Matrix)  return Integer_Matrix;
   function To_Integer_Matrix (IA : Integer_Array) return Integer_Matrix;
   pragma Inline (To_Integer_Matrix);
   function To_Natural_Array (List : NL_Types.Natural_List)
                               return Natural_Array;
   pragma Inline (To_Natural_Array);
   function To_Real_Float_List (M : Real_Float_Matrix; row : Integer)
                                return Real_Float_List;
   function To_Real_Float_List (V : Real_Float_Vector) return Real_Float_List;
   pragma Inline (To_Real_Float_List);
   function To_Real_Float_Matrix (Matrix : Binary_Matrix)
                                   return Real_Float_Matrix;
   function To_Real_Float_Matrix (List : NL_Types.Float_List_2D)
                                   return Real_Float_Matrix;
   function To_Real_Float_Matrix
     (IA : Integer_Array; Data_Axis : Positive := 1)
      return Real_Float_Matrix;
   function To_Real_Float_Matrix
     (List        : Real_Float_List; Data_Axis : Positive := 1;
      First_Index : Integer := 1) return Real_Float_Matrix;
   function To_Real_Float_Matrix (List : Real_Float_List_2D)
                                   return Real_Float_Matrix;
   function To_Real_Float_Matrix (BM : Boolean_Matrix)
                                   return Real_Float_Matrix;
   function To_Real_Float_Matrix (IM : Integer_Matrix)
                                   return Real_Float_Matrix;
   pragma Inline (To_Real_Float_Matrix);
   function To_Real_Float_Vector (List : Binary_Array)
                                  return Real_Float_Vector;
   function To_Real_Float_Vector (List : Integer_Array)
                                  return Real_Float_Vector;
   function To_Real_Float_Vector (List : Real_Float_List)
                                   return Real_Float_Vector;
   pragma Inline (To_Real_Float_Vector);
   function To_Unbound_Array (UB_List : ML_Types.Unbounded_List)
                               return Unbounded_String_Array;
   pragma Inline (To_Unbound_Array);
   function Transpose (Values : Boolean_Matrix) return Boolean_Matrix;
   pragma Inline (Transpose);
   function Transpose (Values : Integer_Matrix) return Integer_Matrix;
   pragma Inline (Transpose);
   function Unit_Float_Matrix (Num_Rows : Positive) return Real_Float_Matrix;
   function Unit_Integer_Matrix (Num_Rows : Positive) return Integer_Matrix;
   function Zero_Array (Num_Rows : Positive) return Real_Float_Vector;
   function Zero_Matrix (Num_Rows, Num_Cols : Positive) return Binary_Matrix;
   function Zero_Matrix (Num_Rows, Num_Cols : Positive) return Integer_Matrix;
   function Zero_Matrix (Num_Rows, Num_Cols : Positive)
                          return Real_Float_Matrix;

end ML_Arrays_And_Matrices;
