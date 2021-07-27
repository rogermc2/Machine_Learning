
with Ada.Containers.Indefinite_Vectors;
with Ada.Containers.Ordered_Maps;
with Ada.Containers.Vectors;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with Maths;

with ML_Types;

package Classifier_Utilities is

   subtype Class_Label is Unbounded_String;
   type Weight_Type is (No_Weight, Balanced_Weight, Weight_Dict, List_Of_Weights);
   type Weight_Data is record
      Label  : Class_Label := To_Unbounded_String ("None");
      Weight : Float := 1.0;
   end record;

   type Integer_Array is array (Integer range <>) of Integer;
   type Float_Array is array (Integer range <>) of Float;

   package Integer_Package is new Ada.Containers.Vectors
     (Positive, Integer);
   subtype Integer_List is Integer_Package.Vector;

   package Integer_Array_Package is new Ada.Containers.Indefinite_Vectors
     (Positive, Integer_Array);
   subtype Integer_Array_List is Integer_Array_Package.Vector;

   package Float_Package is new Ada.Containers.Vectors
     (Positive, Float);
   subtype Float_List is Float_Package.Vector;

   package Probabilities_Package is new Ada.Containers.Vectors (Positive, Float);
   type Probabilities_List is new Probabilities_Package.Vector with null record;

   type Probability_Array is array (Integer range <>) of Float;
   type Sample_Matrix is array (Integer range <>, Integer range <>) of Integer;

   package Weight_Dictionary is new Ada.Containers.Ordered_Maps
     (Class_Label, Float);
   subtype Weight_Map is Weight_Dictionary.Map;

   package Weight_Package is new Ada.Containers.Vectors
     (Positive, Weight_Data);
   subtype Weight_List is Weight_Package.Vector;

   Value_Error : Exception;

   procedure Clear (anArray : in out ML_Types.Label_Data_Array);
   function Compute_Sample_Weight (Class_Weight : Weight_Type;
                                   Y : Integer_Array_List;
                                   Indices      : Integer_List :=
                                     Integer_Package.Empty_Vector;
                                   Weights : Weight_List :=
                                     Weight_Package.Empty_Vector)
                                   return Float_List;
   procedure Print_Integer_Array (Name : String; anArray : Integer_Array);
   procedure Print_Float_Array (Name   : String; anArray : Float_Array;
                                Start, Finish : Integer);
   procedure Print_Float_List (Name  : String; theList : Float_List);
   function To_Float_List (A : Float_Array) return Float_List;
   function To_Array (L : Integer_List) return Integer_Array;
   function Unique_Integer_Array (Nums : ML_Types.Label_Data_Array)
                                  return Integer_Array;
   function Unique_Integer_Array (Nums : Integer_Array) return Integer_Array;
   --  As Integer_List, indices are part of the returned list
   function Unique (Nums : Integer_List) return Integer_List;
   function Unique_Integer (Nums : ML_Types.Label_Data_Array)
                            return Integer_List;

end Classifier_Utilities;
