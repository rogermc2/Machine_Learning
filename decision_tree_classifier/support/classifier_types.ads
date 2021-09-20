
with Ada.Containers.Indefinite_Vectors;
with Ada.Containers.Vectors;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package Classifier_Types is

   subtype Class_Label is Unbounded_String;

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

   package Float_Package is new Ada.Containers.Vectors (Positive, Float);
   subtype Float_List is Float_Package.Vector;
   subtype Weight_List is Float_Package.Vector;

   use Float_Package;
   package Weight_List_Package is new
      Ada.Containers.Vectors (Positive, Weight_List);
   subtype List_Of_Weight_Lists is Weight_List_Package.Vector;

   package Natural_Package is new Ada.Containers.Vectors (Positive, Natural);
   subtype Natural_List is Natural_Package.Vector;
   package Natural_Sorting is new Natural_Package.Generic_Sorting ("<");

   use Natural_Package;
   package Natural_List_Package is new
      Ada.Containers.Vectors (Positive, Natural_List);
   subtype List_Of_Natural_Lists is Natural_List_Package.Vector;

   package Integer_Package is new Ada.Containers.Vectors (Positive, Integer);
   subtype Integer_List is Integer_Package.Vector;

   package Probabilities_Package is new Ada.Containers.Vectors (Positive, Float);
   type Probabilities_List is new Probabilities_Package.Vector with null record;

   type Probability_Array is array (Integer range <>) of Float;

end Classifier_Types;
