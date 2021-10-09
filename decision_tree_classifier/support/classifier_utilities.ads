
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with Classifier_Types; use Classifier_Types;
with Estimator;
with ML_Types;
with Base_Decision_Tree;
with Tree;
with Weights;

package Classifier_Utilities is

    Value_Error : Exception;

    function Bin_Count (Numbers : Natural_List) return Natural_List;
    function Bin_Count (Numbers : ML_Types.Value_Data_List) return Natural_List;
    procedure Clear (anArray : in out ML_Types.Value_Data_Array);
    function Compare_Float_Lists (L, R : Float_List) return Boolean;
    function Dot (L : Weights.Weight_List; R : Natural_List) return Float;
    procedure Print_Boolean_Matrix (Name    : String;
                                    aMatrix : Estimator.Boolean_Matrix);
    procedure Print_Integer_Array (Name : String; anArray : Integer_Array);
    procedure Print_Float_Array (Name          : String; anArray : Float_Array;
                                 Start, Finish : Integer);
    procedure Print_Float_List (Name  : String; theList : Float_List);
    procedure Print_Integer_List (Name : String; theList : Integer_List);
    procedure Print_List_Of_Natural_Lists (Name : String;
                                           Data : List_Of_Natural_Lists);
    procedure Print_List_Of_Float_Lists (Name : String;
                                           Data : List_Of_Float_Lists);
    procedure Print_Multi_Value_Array (Name    : String;
                                       anArray : Multi_Value_Array);
    procedure Print_List_Of_Value_Lists
      (Name : String; Multi_List : ML_Types.List_Of_Value_Data_Lists);
    procedure Print_Natural_List (Name : String; theList : Natural_List);
    procedure Print_Tree (Name : String; aTree : Base_Decision_Tree.Classifier);
    procedure Print_Tree (Name  : String; aTree : Tree.Tree_Class);
    procedure Print_Value_List (Name    : String;
                                theList : ML_Types.Value_Data_List);
    procedure Print_Weights (Name : String; Data : Weights.Weight_List);
    procedure Print_Weights_Lists (Name : String;
                                   Data : Weights.Weight_Lists_List);
    function Search_Sorted_Value_List (List_A, List_B : ML_Types.Value_Data_List)
                                      return Integer_List;
    function Set_Diff (Values, Uniques : Natural_List) return Natural_List;
    function To_Array (L : Integer_List) return Integer_Array;
    function To_Float_List (A : Float_Array) return Float_List;
    function To_Integer_List (A : Integer_Array) return Integer_List;
    function To_Natural_List (A : Natural_Array) return Natural_List;
    function To_Integer_Value_List (A : Integer_Array)
                                   return ML_Types.List_Of_Value_Data_Lists;
    function To_Multi_Value_List (A : Multi_Value_Array)
                                 return ML_Types.List_Of_Value_Data_Lists;
    function To_Natural_Value_List (A : Natural_Array)
                                   return ML_Types.List_Of_Value_Data_Lists;
    function Unique_Integer_Array (Nums : ML_Types.Value_Data_Array)
                                  return Integer_Array;
    function Unique_Integer_Array (Nums : Integer_Array) return Integer_Array;
    --  As Integer_List, indices are part of the returned list
    function Unique (Nums : Integer_List) return Integer_List;

end Classifier_Utilities;
