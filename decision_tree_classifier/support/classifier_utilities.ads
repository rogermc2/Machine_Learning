
with Classifier_Types; use Classifier_Types;
with ML_Types;
with Tree;
with Weights;

package Classifier_Utilities is

    Value_Error : Exception;

    function Bin_Count (Numbers : Natural_List) return Natural_List;
    function Bin_Count (Numbers : ML_Types.Value_Data_List) return Natural_List;
    procedure Clear (anArray : in out ML_Types.Value_Data_Array);
    function Compare_Float_Lists (L, R : Float_List) return Boolean;
    function Dot (L : Weights.Weight_List; R : Natural_List) return Float;
    function Get_Column (List_2D      : Float_List_2D;
                         Column_Index : Positive) return Float_List;
    function Init_Samples_Copy (Samples : ML_Types.Value_Data_Lists_2D)
                               return ML_Types.Value_Data_Lists_2D;
    function Nodes_3D_To_Outputs_3D (Nodes : Weights.Weight_Lists_3D;
                                     Num_Outputs : Positive)
                                     return Weights.Weight_Lists_3D;
    function Search_Sorted_Value_List
      (List_A, List_B : ML_Types.Value_Data_List) return Integer_List;
    function Set_Diff (Values, Uniques : Natural_List) return Natural_List;
    function Sum_Cols (aList : Classifier_Types.Float_List_2D)
                      return Classifier_Types.Float_List;
    function Sum_Cols (aList : ML_Types.Value_Data_Lists_2D)
                      return ML_Types.Value_Data_List;
    function To_Array (L : Integer_List) return Integer_Array;
    function To_Float_List (A : Float_Array) return Float_List;
    function To_Integer_List (A : Integer_Array) return Integer_List;
    function To_Natural_List (A : Natural_Array) return Natural_List;
    function To_Integer_Value_List (A : Integer_Array)
                                   return ML_Types.Value_Data_List;
    function To_Integer_Value_List_2D (A : Integer_Array)
                                      return ML_Types.Value_Data_Lists_2D;
    function To_Multi_Value_List (A : Multi_Value_Array)
                                 return ML_Types.Value_Data_Lists_2D;
    function To_Natural_Value_List (A : Natural_Array)
                                   return ML_Types.Value_Data_Lists_2D;
    function Transpose (Values : ML_Types.Value_Data_Lists_2D)
                        return  ML_Types.Value_Data_Lists_2D;
    function Traverse_Tree (Current_Node : Tree.Tree_Cursor)
                           return Tree.Tree_Cursor;
    function Unique_Integer_Array (Nums : ML_Types.Value_Data_Array)
                                  return Integer_Array;
    function Unique_Integer_Array (Nums : Integer_Array) return Integer_Array;
    --  As Integer_List, indices are part of the returned list
    function Unique (Nums : Integer_List) return Integer_List;

end Classifier_Utilities;
