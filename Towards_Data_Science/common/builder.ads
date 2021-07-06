
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
    function Build_Tree2 (Rows : Rows_Vector) return Tree_Type;
    function Classify (aRow : Row_Data; Node_Cursor : Tree_Cursor)
                       return ML_Types.Prediction_Data_List;
    function UB_Label_Counts (Rows : Rows_Vector) return UB_Label_Map;
    function Find_Best_Split (Rows : Rows_Vector) return Best_Data;
    function Gain (Data : Best_Data) return Float;
    function Gini (Rows : Rows_Vector) return Float;
    function Header_Row return Header_Data_Type;
    function Information_Gain (Left, Right : Rows_Vector;
                               Current_Uncertainty : Float) return float;
    function Initialize (Rows : Data_Rows) return Rows_Vector;
    function Match (Question : Question_Data;
                    Example_Data : Row_Data) return Boolean;
    function Num_Features (aString : String) return Class_Range;
    function Partition (Rows : Rows_Vector; aQuestion : Question_Data)
                        return Partitioned_Rows;
    function To_Question (Q : Raw_Question) return Question_Data;
private

    type Best_Data is record
        Question   : Question_Data;
        True_Rows  : Rows_Vector;
        False_Rows : Rows_Vector;
        Gain       : Float := 0.0;
    end record;

end Builder;
