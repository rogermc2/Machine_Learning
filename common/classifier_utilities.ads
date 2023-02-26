
with Base_Decision_Tree;
with ML_Arrays_And_Matrices; use ML_Arrays_And_Matrices;
with ML_Types;
with NL_Types;
with Tree;
with Weights;

package Classifier_Utilities is

   type Feature_Type_Array is array (Positive range <>) of ML_Types.Data_Type;
   type Label_Type_Array is array (Positive range <>) of ML_Types.Data_Type;

   Value_Error : exception;

   function Arg_Max (Values : Weights.Weight_List) return Positive;
   function Arg_Max (Values_2D : Weights.Weight_Lists_2D; Axis : Natural := 0)
                      return NL_Types.Natural_List;
   function Arg_Max (Values_3D : Weights.Weight_Lists_3D; Axis : Natural := 0)
                      return NL_Types.Natural_List;
   function Bin_Count (Numbers : NL_Types.Natural_List)
                       return NL_Types.Natural_List;
   function Bin_Count (Numbers : ML_Types.Value_Data_List)
                       return NL_Types.Natural_List;
   procedure Clear (anArray : in out ML_Types.Value_Data_Array);
   function Compare_Float_Lists (L, R : NL_Types.Float_List) return Boolean;
   function Count_Samples (aClassifier : Base_Decision_Tree.Classifier)
                            return Natural;
   function Dot (L : Weights.Weight_List; R : NL_Types.Natural_List)
                 return Float;
   function Get_Column (List_2D      : NL_Types.Float_List_2D;
                        Column_Index : Positive) return NL_Types.Float_List;
   function Float_Precision (Number : Float; Precision : Natural)
                             return String;
   function Init_Samples_Copy (Samples : ML_Types.Value_Data_Lists_2D)
                                return ML_Types.Value_Data_Lists_2D;
   function Load_Data (File_Name : String; Num_Outputs : Positive := 1;
                       Max_Lines : Positive := 20000)
                       return ML_Types.Multi_Output_Data_Record;
   function Max_Probability_Indices (Probabilities : Binary_Matrix)
                                     return Integer_Array;
   function Probabilities (aMatrix : Real_Float_Matrix)
                           return Real_Float_Matrix;
   function Max_Probability_Indices (Probabilities : Real_Float_Matrix)
                                     return Integer_Array;
   function Probabilities (Vec : Real_Float_Vector) return Real_Float_Vector;
   function Samples_3D_To_Outputs_3D (Samples     : Weights.Weight_Lists_3D;
                                      Num_Outputs : Positive)
                                       return Weights.Weight_Lists_3D;
   function Ones (List_Length : Positive) return Weights.Weight_List;
   function Row_Max_Indices (Values : Boolean_Matrix) return Natural_Array;
   function Row_Max_Indices (Values : Real_Float_Matrix) return Natural_Array;
   function Search_Sorted_Float_List (List_A, List_B : NL_Types.Float_List)
                                      return ML_Types.Integer_List;
   function Search_Sorted_Integer_List (List_A, List_B : ML_Types.Integer_List)
                                        return ML_Types.Integer_List;
   function Search_Sorted_Value_List
     (List_A, List_B : ML_Types.Value_Data_List)
       return ML_Types.Integer_List;
   function Set_Diff (Values : Integer_Array; Uniques : Integer_Array)
                      return NL_Types.Natural_List;
   function Set_Diff (Values, Uniques : NL_Types.Natural_List)
                      return NL_Types.Natural_List;
   function Set_Diff (Values : Integer_Array; Uniques : Natural_Array)
                      return NL_Types.Natural_List;
   function Set_Diff (Values : Natural_Array; Uniques : Integer_Array)
                      return NL_Types.Natural_List;
   function Set_Diff (Values, Uniques : Boolean_Array)
                      return NL_Types.Boolean_List;
   function Set_Value (List_Length : Positive; Value : Float)
                        return Weights.Weight_List;
   function Sum_Cols (aList : NL_Types.Float_List_2D)
                       return NL_Types.Float_List;
   function Sum_Cols (aList : ML_Types.Value_Data_Lists_2D)
                       return ML_Types.Value_Data_List;
   function Sum_Cols (aList : Weights.Weight_Lists_3D)
                       return Weights.Weight_List;
   function To_Array (L : ML_Types.Integer_List) return Integer_Array;
   function To_Float_List (A : Float_Array) return NL_Types.Float_List;
   function To_Float_List (F : ML_Types.Value_Data_List)
                           return NL_Types.Float_List;
   function To_Float_List_2D (Data : ML_Types.Value_Data_Lists_2D)
                              return NL_Types.Float_List_2D;
   function To_Float_List_2D (I : ML_Types.Integer_List_2D)
                              return NL_Types.Float_List_2D;
   function To_Integer_List_2D (Data : ML_Types.Value_Data_Lists_2D)
                                return ML_Types.Integer_List_2D;
   function To_Integer_List (A : Integer_Array) return ML_Types.Integer_List;
   function To_Integer_List (Ints : ML_Types.Value_Data_List)
                              return ML_Types.Integer_List;
   function To_Integer_Value_List (A : Integer_Array)
                                    return ML_Types.Value_Data_List;
   function To_Integer_Value_List_2D (A : Integer_Array)
                                       return ML_Types.Value_Data_Lists_2D;
   function To_Multi_Value_List (A : Multi_Value_Array)
                                  return ML_Types.Value_Data_Lists_2D;
   function To_Natural_List (A : Natural_Array) return NL_Types.Natural_List;
   function To_Natural_List (Numbers : ML_Types.Value_Data_List)
                              return NL_Types.Natural_List;
   function To_Natural_Value_List (A : Natural_Array)
                                    return ML_Types.Value_Data_Lists_2D;
   function To_PL_Array (List_1D : NL_Types.Float_List; Num_Rows : Positive)
                         return Real_Float_Matrix;
   function To_Value_2D_List (A : ML_Types.Value_Data_List)
                               return ML_Types.Value_Data_Lists_2D;
   function To_Value_2D_List (List_1D  : ML_Types.Value_Data_List;
                              Num_Rows : Positive)
                               return ML_Types.Value_Data_Lists_2D;
   function Transpose (Values : ML_Types.Value_Data_Lists_2D)
                        return  ML_Types.Value_Data_Lists_2D;
   function Traverse_Tree (Current_Node : Tree.Tree_Cursor)
                            return Tree.Tree_Cursor;
   function Unique_Integer_Array (Nums : ML_Types.Value_Data_Array)
                                   return Integer_Array;
   function Unique_Integer_Array (Nums : Integer_Array) return Integer_Array;
   function Unique (Nums : ML_Types.Integer_List) return ML_Types.Integer_List;
   function Unique_Values (Values : ML_Types.Value_Data_List)
                            return ML_Types.Value_Data_List;
   function Unique_Weights (Values : Weights.Weight_Lists_3D)
                             return Weights.Weight_List;

end Classifier_Utilities;
