
with Ada.Containers.Indefinite_Ordered_Maps;
with Ada.Containers.Vectors;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package Support is

   type Colour_Type is (Green, Yellow, Red);
   type Label_Type is (Apple, Grape, Lemon, Grapefruit, Orange, Blueberry);

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

   function Class_Counts (Rows : Support.Rows_Vector)
                          return Support.Count_Package.Map;
   function Gini (Rows : Rows_Vector) return Float;
   function Information_Gain (Left, Right : Rows_Vector;
                              Current_Uncertainty : Float) return float;
   procedure Print_Question (Self : Question_Type);
   procedure Print_Rows (Label : String; Rows : Rows_Vector);
   function To_Vector (Rows : Row_Array) return Rows_Vector;

end Support;
