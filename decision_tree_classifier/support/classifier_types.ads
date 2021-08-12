
with Ada.Containers.Indefinite_Vectors;
with Ada.Containers.Vectors;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package Classifier_Types is

   subtype Class_Label is Unbounded_String;

   type Integer_Array is array (Integer range <>) of Integer;
   type Float_Array is array (Integer range <>) of Float;

   package Integer_Array_Package is new Ada.Containers.Indefinite_Vectors
     (Positive, Integer_Array);
   subtype Integer_Array_List is Integer_Array_Package.Vector;

   package Boolean_Package is new Ada.Containers.Vectors
     (Positive, Boolean);
   subtype Boolean_List is Boolean_Package.Vector;

   package Float_Package is new Ada.Containers.Vectors (Positive, Float);
   subtype Float_List is Float_Package.Vector;
   subtype Weight_List is Float_Package.Vector;

   package Natural_Package is new Ada.Containers.Vectors (Positive, Natural);
   subtype Natural_List is Natural_Package.Vector;

   package Integer_Package is new Ada.Containers.Vectors (Positive, Integer);
   subtype Integer_List is Integer_Package.Vector;

   package Probabilities_Package is new Ada.Containers.Vectors (Positive, Float);
   type Probabilities_List is new Probabilities_Package.Vector with null record;

   type Probability_Array is array (Integer range <>) of Float;
   type Sample_Matrix is array (Integer range <>, Integer range <>) of Integer;

end Classifier_Types;
