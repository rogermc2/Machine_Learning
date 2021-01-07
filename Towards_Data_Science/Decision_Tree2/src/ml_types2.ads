
with Ada.Containers.Doubly_Linked_Lists;
with Ada.Containers.Indefinite_Multiway_Trees;
with Ada.Containers.Indefinite_Ordered_Maps;
with Ada.Containers.Ordered_Maps;
with Ada.Containers.Vectors;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package ML_Types2 is

   Max_Features : constant Integer := 100;
   type Class_Range is new Positive range 1 .. Max_Features;
   type Features_Name_Array is array (Class_Range range <>) of Unbounded_String;
   type Features_ID_Array is array (Class_Range range <>) of Positive;
   subtype Feature_Class is Class_Range;
   subtype Label_Type is Positive;
   subtype Question_Type is Class_Range;

   type Node_Kind is (Decision_Kind, Prediction_Kind);

   type Row_Data (Class_Count : Class_Range := 2) is record
      Features : Features_Name_Array (1 .. Class_Count);
      Label    : Unbounded_String;
   end record;

   package Rows_Package is new Ada.Containers.Vectors (Positive, Row_Data);
   subtype Rows_Vector is Rows_Package.Vector;

   type Row_Array is array (Integer range <>) of Row_Data (2);

   type Features_Data (Class_Count : Class_Range := 2) is record
      Features   : Features_ID_Array (1 .. Class_Count);
   end record;

   package Count_Package is new Ada.Containers.Indefinite_Ordered_Maps
     (Label_Type, Natural);

   package Feature_Map_Package is new Ada.Containers.Ordered_Maps
     (Unbounded_String, Class_Range);
   subtype Feature_Map is Feature_Map_Package.Map;

   type Feature_Type is (Integer_Type, Float_Type, Boolean_Type);
   package Feature_Type_Package is new Ada.Containers.Ordered_Maps
     (Class_Range, Feature_Type);
   subtype Feature_Type_Map is Feature_Type_Package.Map;

   type Question (Column_Type : Feature_Type := Integer_Type) is record
      Column   : Class_Range;
      case Column_Type is
         when Integer_Type => Integer_Value : Integer;
         when Float_Type => Float_Value   : Float;
         when Boolean_Type => Boolean_Value : Boolean;
      end case;
   end record;

   type Decision_Node_Type (Node_Type : Node_Kind := Decision_Kind) is record
      case Node_Type is
      when  Decision_Kind =>
         Question    : Question_Type;
         True_Rows   : Rows_Vector := Rows_Package.Empty_Vector;
         False_Rows  : Rows_Vector := Rows_Package.Empty_Vector;
      when Prediction_Kind =>
         Predictions : Count_Package.Map := Count_Package.Empty_Map;
      end case;
   end record;

   type Best_Split_Data is record
      Best_Gain     : Float;
      Best_Question : Question_Type;
   end record;

   type Partitioned_Rows is record
      True_Rows  : Rows_Vector;
      False_Rows : Rows_Vector;
   end record;

   package Tree_Package is new Ada.Containers.Indefinite_Multiway_Trees
     (Decision_Node_Type);
   subtype Tree_Type is Tree_Package.Tree;
   subtype Tree_Cursor is Tree_Package.Cursor;

   package Strings_Package is new Ada.Containers.Doubly_Linked_Lists
     (Unbounded_String);
--     subtype Strings_List is Strings_Package.List;

end ML_Types2;
