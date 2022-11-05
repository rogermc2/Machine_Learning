
with Ada.Containers.Doubly_Linked_Lists;
with Ada.Containers.Vectors;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with ML_Types;

package NL_Types is
   pragma Preelaborate;

   Max_Features : constant Integer := 100;

   type Data_Type is (Integer_Type, Float_Type, Boolean_Type, UB_String_Type);
   pragma Ordered (Data_Type);

   type Class_Range is new Positive range 1 .. Max_Features;
   subtype Feature_Name_Type is Unbounded_String;
   type Feature_Names_Array is array (Class_Range range <>)
     of Feature_Name_Type;
   type Feature_Data_Array is array (Class_Range range <>) of Unbounded_String;
   type Data_Array is array (Class_Range range <>) of Unbounded_String;
   type Data_Type_Array is array (Class_Range range <>) of Data_Type;

   package Float_Package is new Ada.Containers.Vectors (Positive, Float);
   subtype Float_List is Float_Package.Vector;
   package Float_Sorting is new Float_Package.Generic_Sorting ("<");
   function ">" (L, R : Float_List) return Boolean;
   pragma Inline (">");
   function "+" (L, R : Float_List) return Float_List;
   pragma Inline ("+");
   function "-" (L, R : Float_List) return Float_List;
   pragma Inline ("-");
   function "*" (L : Float; R : Float_List) return Float_List;
   pragma Inline ("*");
   function "*" (L, R : Float_List) return Float_List;
   pragma Inline ("*");
   function "**" (L : Float_List; P : Integer) return Float_List;
   pragma Inline ("**");
   function "/" (L : Float_List; R : Float) return Float_List;
   pragma Inline ("/");
   function "abs" (aVector : Float_List) return Float_List;
   pragma Inline ("abs");
   procedure Check_Lengths (Routine_Name : String; L, R : Float_List);
   pragma Inline (Check_Lengths);

   use Float_Package;
   package Float_List_Package is new
     Ada.Containers.Vectors (Positive, Float_List);
   subtype Float_List_2D is Float_List_Package.Vector;
--     function ">" (L, R : Float_List_Package.Vector) return Boolean;
   function ">" (L, R : Float_List_2D) return Boolean;
   pragma Inline (">");
   function "*" (L : Float; R : Float_List_2D) return Float_List_2D;
   pragma Inline ("*");
   function "*" (L, R : Float_List_2D) return Float_List_2D;
   pragma Inline ("*");
   function "**" (L : Float_List_2D; P : Integer) return Float_List_2D;
   pragma Inline ("**");
   function "/" (L : Float_List_2D; R : Float) return Float_List_2D;
   pragma Inline ("/");
   function "+" (L, R : Float_List_2D) return Float_List_2D;
   pragma Inline ("+");
   function "-" (L, R : Float_List_2D) return Float_List_2D;
   pragma Inline ("-");
   function Dot (L : Float_List; R : Float_List_2D) return Float_List;
   pragma Inline (Dot);
   function Dot (L, R : Float_List) return Float_List_2D;
   pragma Inline (Dot);
   function Dot (L, R : Float_List_2D) return Float_List_2D;
   pragma Inline (Dot);
   function Transpose (Values : Float_List_2D) return  Float_List_2D;
   pragma Inline (Transpose);

   use Float_List_Package;
   package List_Of_Float_Lists_Package is new
     Ada.Containers.Vectors (Positive, Float_List_2D);
   subtype Float_List_3D is List_Of_Float_Lists_Package.Vector;

   procedure Check_Lengths (Routine_Name : String; L : ML_Types.Integer_List;
                            R            : Float_List);

   package Integer_DLL_Package is new
     Ada.Containers.Doubly_Linked_Lists (Integer);
   subtype Integer_DL_List is Integer_DLL_Package.List;

   package Boolean_Package is new Ada.Containers.Vectors
     (Positive, Boolean);
   subtype Boolean_List is Boolean_Package.Vector;
   package Boolean_Sorting is new Boolean_Package.Generic_Sorting ("<");

   use Boolean_Package;
   package Boolean_Package_2D is new
     Ada.Containers.Vectors (Positive, Boolean_List);
   subtype Boolean_List_2D is Boolean_Package_2D.Vector;

   package Natural_Package is new Ada.Containers.Vectors (Positive, Natural);
   subtype Natural_List is Natural_Package.Vector;
   subtype Natural_Cursor is Natural_Package.Cursor;
   package Natural_Sorting is new Natural_Package.Generic_Sorting ("<");

   use Natural_Package;
   package Natural_List_Package is new
     Ada.Containers.Vectors (Positive, Natural_List);
   subtype Natural_Lists_2D is Natural_List_Package.Vector;

   type Slice_Record is record
      First : Positive;
      Last  : Positive;
   end record;

   package Slices_Package is new Ada.Containers.Vectors
     (Positive, Slice_Record);
   subtype Slices_List is Slices_Package.Vector;

end NL_Types;
