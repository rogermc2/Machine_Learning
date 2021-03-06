
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with ML_Types; use ML_Types;

package Builder is

--     package Vector_Rows_Package is new Ada.Containers.Vectors (Positive, Vector_Row_Data);
--     subtype Rows_Vector_Vector is Vector_Rows_Package.Vector;

--     type Value_Data (Feature : Feature_Type) is record
--        case Feature is
--           when Colour_Feature => Colour : Colour_Type;
--           when Diameter_Feature => Diameter : Positive;
--        end case;
--     end record;
--
--     package Value_Set_Package is new
--       Ada.Containers.Indefinite_Doubly_Linked_Lists (Value_Data);
--     subtype Value_Set is Value_Set_Package.List;

--     function Build_Tree (Rows : Rows_Vector) return Tree_Type;
--     function Classify (aRow : Row_Data; aTree : Tree_Type)
--                           return Count_Package.Map;
   function Class_Counts (Rows : Rows_Vector) return Count_Package.Map;
--     procedure Evaluate (Rows : Rows_Vector; theTree : Tree_Type);
--     function Find_Best_Split (Rows : Rows_Vector) return Best_Split_Data;
   function Gini (Rows : Rows_Vector) return Float;
   function Information_Gain (Left, Right : Rows_Vector;
                              Current_Uncertainty : Float) return float;
   function Match (Self    : Question_Data;
                   Example : Row_Data) return Boolean;
   function Partition (Rows : Rows_Vector; aQuestion : Question_Data)
                       return Partitioned_Rows;
   procedure Print_Classification (Classification : Count_Package.Map);
   procedure Print_Class_Counts (Rows : Rows_Vector);
   function Print_Leaf (Counts : Count_Package.Map) return String;
   procedure Print_Question (Self : Raw_Question);
   procedure Print_Rows (Label : String; Rows : Rows_Vector);
   procedure Print_Tree (aTree : Tree_Package.Tree);
--     procedure Print_Unique_Values (Rows    : Rows_Vector;
--                                    Feature : Feature_Class);
   function To_Question (Q : Raw_Question) return Question_Data;
   function To_Vector (Rows : Row_Array; Header_Row : out Header_Data)
                       return Rows_Vector;
--     function Unique_Values (Rows    : Rows_Vector;
--                             Feature : Feature_Class) return Value_Set;
end Builder;
