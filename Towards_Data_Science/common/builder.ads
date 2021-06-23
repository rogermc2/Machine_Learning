
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

    type Best_Data is private;

    Builder_Exception : exception;

    function Best_Question (Data : Best_Data) return Question_Data;
    function Build_Tree (Rows : Rows_Vector) return Tree_Type;
    --     function Classify (aRow : Row_Data; aTree : Tree_Type)
    --                           return Count_Package.Map;
    function UB_Class_Counts (Rows : Rows_Vector) return UB_Label_Map;
    --     procedure Evaluate (Rows : Rows_Vector; theTree : Tree_Type);
    function Find_Best_Split (Rows : Rows_Vector) return Best_Data;
    function Gain (Data : Best_Data) return Float;
    function Gini (Rows : Rows_Vector) return Float;
    function Header_Row return Row_Data;
    function Information_Gain (Left, Right : Rows_Vector;
                               Current_Uncertainty : Float) return float;
    function Match (Question : Question_Data;
                    Example : Row_Data) return Boolean;
    function Num_Features (aString : String) return Class_Range;
    function Partition (Rows : Rows_Vector; aQuestion : Question_Data)
                        return Partitioned_Rows;
    function To_Question (Q : Raw_Question) return Question_Data;
    function To_Rows_Vector (Rows : Data_Rows) return Rows_Vector;
private

    type Best_Data is record
        Question : Question_Data;
        Gain     : Float := 0.0;
    end record;

end Builder;
