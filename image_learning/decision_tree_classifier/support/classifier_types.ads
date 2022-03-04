
with Ada.Containers.Indefinite_Vectors;
with Ada.Containers.Vectors;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package Classifier_Types is

   subtype Class_Label is Unbounded_String;

   type Index_Range_2D is new Integer range 1 .. 2;
   type Index_Array_2D is array (Index_Range_2D range <>) of Positive;

   type Integer_Array is array (Integer range <>) of Integer;
   type Float_Array is array (Integer range <>) of Float;
   type Natural_Array is array (Integer range <>) of Natural;
   type Multi_Value_Array is array (Integer range <>, Integer range <>)
     of Integer;

   package Integer_Array_Package is new Ada.Containers.Indefinite_Vectors
     (Positive, Integer_Array);
   subtype Integer_Array_List is Integer_Array_Package.Vector;

   package Boolean_Package is new Ada.Containers.Vectors
     (Positive, Boolean);
   subtype Boolean_List is Boolean_Package.Vector;

   package Natural_Package is new Ada.Containers.Vectors (Positive, Natural);
   subtype Natural_List is Natural_Package.Vector;
   subtype Natural_Cursor is Natural_Package.Cursor;
   package Natural_Sorting is new Natural_Package.Generic_Sorting ("<");

   use Natural_Package;
   package Natural_List_Package is new
      Ada.Containers.Vectors (Positive, Natural_List);
   subtype Natural_Lists_2D is Natural_List_Package.Vector;

   package Probabilities_Package is new Ada.Containers.Vectors (Positive, Float);
   type Probabilities_List is new Probabilities_Package.Vector with null record;

   type Probability_Array is array (Integer range <>) of Float;

end Classifier_Types;
