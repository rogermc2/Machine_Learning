
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with ML_Types; use ML_Types;

package Builder is

    type Best_Data is private;

    Builder_Exception : exception;

    function Best_Question (Data : Best_Data) return Question_Data;
    function Build_Tree (Rows : Rows_Vector) return Tree_Type;
    function Classify (aRow : Row_Data; Node_Cursor : Tree_Cursor)
                       return ML_Types.Prediction_Data_List;
    procedure Evaluate (Data : Rows_Vector; aTree : Tree_Type);
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
    function UB_Label_Counts (Rows : Rows_Vector) return UB_Label_Map;

private

    type Best_Data is record
        Question   : Question_Data;
        True_Rows  : Rows_Vector;
        False_Rows : Rows_Vector;
        Gain       : Float := 0.0;
    end record;

end Builder;
