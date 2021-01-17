
with Ada.Containers.Indefinite_Doubly_Linked_Lists;
with Ada.Containers.Indefinite_Multiway_Trees;
with Ada.Containers.Indefinite_Ordered_Maps;
with Ada.Containers.Vectors;
with ML_Types; use ML_Types;

package Builder is

   type Feature_Type is (Colour_Feature, Diameter_Feature);
   type Colour_Type is (Green, Yellow, Red);
   type Label_Type is (Apple, Grape, Lemon, Grapefruit, Orange, Blueberry);

   type Row_Data (Class_Count : Integer := 2) is record
      Colour   : Colour_Type;
      Diameter : Positive;
      Fruit    : Label_Type;
   end record;

   package Rows_Package is new Ada.Containers.Vectors (Positive, Row_Data);
   subtype Rows_Vector is Rows_Package.Vector;

   type Row_Array is array (Integer range <>) of Row_Data (2);

   type Features_Data is record
      Colour   : Colour_Type;
      Diameter : Positive;
   end record;

    type Vector_Row_Data is record
      Features : Features_Data;
      Label    : Label_Type;
    end record;

   package Vector_Rows_Package is new Ada.Containers.Vectors (Positive, Vector_Row_Data);
   subtype Rows_Vector_Vector is Vector_Rows_Package.Vector;

   type Question_Type (Feature : Feature_Type := Colour_Feature) is record
      case Feature is
         when Colour_Feature => Colour_Value     : Colour_Type;
         when Diameter_Feature => Diameter_Value : Integer;
      end case;
   end record;

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

   type Value_Data (Feature : Feature_Type) is record
      case Feature is
         when Colour_Feature => Colour : Colour_Type;
         when Diameter_Feature => Diameter : Positive;
      end case;
   end record;

   package Value_Set_Package is new
     Ada.Containers.Indefinite_Doubly_Linked_Lists (Value_Data);
   subtype Value_Set is Value_Set_Package.List;

   function Build_Tree (Rows : Rows_Vector) return Tree_Type;
   function Classify (aRow : Row_Data; aTree : Tree_Type)
                         return Count_Package.Map;
   function Class_Counts (Rows : Rows_Vector) return Count_Package.Map;
   procedure Evaluate (Rows : Rows_Vector; theTree : Tree_Type);
   function Find_Best_Split (Rows : Rows_Vector) return Best_Split_Data;
   function Gini (Rows : Rows_Vector) return Float;
   function Information_Gain (Left, Right : Rows_Vector;
                              Current_Uncertainty : Float) return float;
   procedure Print_Rows (Label : String; Rows : Rows_Vector);
   function Match (Self    : Question_Type;
                   Example : Row_Data) return Boolean;
   function Partition (Rows     : Rows_Vector;
                       Question : Question_Type)
                          return Partitioned_Rows;
   procedure Print_Classification (Classification : Count_Package.Map);
   procedure Print_Class_Counts (Rows : Rows_Vector);
   function Print_Leaf (Counts : Count_Package.Map) return String;
   procedure Print_Question (Self : Question_Type);
   procedure Print_Tree (aTree : Tree_Package.Tree);
   procedure Print_Unique_Values (Rows    : Rows_Vector;
                                  Feature : Feature_Type);
   function To_Vector (Rows : Row_Array) return Rows_Vector;
   function Unique_Values (Rows    : Rows_Vector;
                           Feature : Feature_Type) return Value_Set;
end Builder;
