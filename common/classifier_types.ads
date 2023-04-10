
--  with Ada.Containers.Indefinite_Vectors;
with Ada.Containers.Vectors;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package Classifier_Types is

   subtype Class_Label is Unbounded_String;

   type Index_Range_2D is new Integer range 1 .. 2;
   type Index_Array_2D is array (Index_Range_2D range <>) of Positive;

   type Multi_Value_Array is array (Integer range <>, Integer range <>)
     of Integer;

   package Boolean_Package is new Ada.Containers.Vectors
     (Positive, Boolean);
   subtype Boolean_List is Boolean_Package.Vector;

   package Integer_Package is new Ada.Containers.Vectors (Positive, Integer);
   subtype Integer_List is Integer_Package.Vector;

   package Probabilities_Package is new Ada.Containers.Vectors (Positive, Float);
   type Probabilities_List is new Probabilities_Package.Vector with null record;

   type Probability_Array is array (Integer range <>) of Float;

end Classifier_Types;
