
with Ada.Containers.Indefinite_Multiway_Trees;
with Ada.Containers.Indefinite_Ordered_Maps;
with Ada.Containers.Vectors;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package ML_Types is

   type Colour_Type is (Green, Yellow, Red);
   type Label_Type is (Apple, Grape, Lemon, Grapefruit, Orange, Blueberry);
   type Node_Kind is (Decision_Kind, Prediction_Kind);

   type Row_Data (Class_Count : Integer := 2) is record
      Colour   : Colour_Type;
      Diameter : Positive;
      Fruit    : Label_Type;
   end record;

   type Row_Array is array (Integer range <>) of Row_Data (2);

   type Feature_Type is (Colour_Feature, Diameter_Feature);

   type Question_Type (Feature : Feature_Type := Colour_Feature) is record
      case Feature is
         when Colour_Feature => Colour_Value     : Colour_Type;
         when Diameter_Feature => Diameter_Value : Integer;
      end case;
   end record;

   package Rows_Package is new Ada.Containers.Vectors (Positive, Row_Data);
   subtype Rows_Vector is Rows_Package.Vector;

   type Header_Type is array (Integer range <>) of Unbounded_String;
   Header : constant Header_Type (1 ..3 ) :=
              (To_Unbounded_String ("Colour"),
               To_Unbounded_String ("diameter"),
               To_Unbounded_String ("Label"));

   package Count_Package is new Ada.Containers.Indefinite_Ordered_Maps
     (Label_Type, Natural);

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

   package Tree_Package is new Ada.Containers.Indefinite_Multiway_Trees
     (Decision_Node_Type);
   subtype Tree_Cursor is Tree_Package.Cursor;
   subtype Tree_Type is Tree_Package.Tree;

end ML_Types;
