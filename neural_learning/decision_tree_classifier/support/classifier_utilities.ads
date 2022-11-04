
with ML_Types;
with NL_Arrays_And_Matrices; use NL_Arrays_And_Matrices;
with NL_Types;

package Classifier_Utilities is

   type Feature_Type_Array is array (Positive range <>) of ML_Types.Data_Type;
   type Label_Type_Array is array (Positive range <>) of ML_Types.Data_Type;

   Value_Error : exception;

   function Arg_Max (Values : Boolean_Array) return Positive;
   function Arg_Max (Values : NL_Types.Boolean_List) return Positive;
   function Arg_Max (Values : NL_Types.Float_List) return Positive;
   function Arg_Max (Values : Real_Float_Matrix) return Integer_Array;
   function Arg_Max (Values : NL_Types.Integer_List) return Positive;
   function Bin_Count (Numbers : NL_Types.Natural_List)
                       return NL_Types.Natural_List;
   function Bin_Count (Numbers : ML_Types.Value_Data_List)
                       return NL_Types.Natural_List;
   function Compare_Float_Lists (L, R : NL_Types.Float_List)
                                 return Boolean;
   function Get_Column (List_2D      : NL_Types.Float_List_2D;
                        Column_Index : Positive) return NL_Types.Float_List;
   function Float_Precision (Number : Float; Precision : Natural) return String;
   function Init_Samples_Copy (Samples : ML_Types.Value_Data_Lists_2D)
                               return ML_Types.Value_Data_Lists_2D;
   function Load_Data (File_Name : String; Num_Outputs : Positive := 1)
                       return ML_Types.Multi_Output_Data_Record;
   function Max_Probability_Indices
     (Probabilities : NL_Arrays_And_Matrices.Binary_Matrix)
      return NL_Arrays_And_Matrices.Integer_Array;
   function Max_Probability_Indices
     (Probabilities : Real_Float_Matrix)
      return NL_Arrays_And_Matrices.Integer_Array;
   function Probabilities (aMatrix : Real_Float_Matrix)
                           return Real_Float_Matrix;
   function Probabilities (Vec : Real_Float_Vector)
                           return Real_Float_Vector;
   function Row_Max_Indices (Values : NL_Arrays_And_Matrices.Boolean_Matrix)
                             return NL_Arrays_And_Matrices.Natural_Array;
   function Row_Max_Indices (Values : Real_Float_Matrix)
                             return NL_Arrays_And_Matrices.Natural_Array;
   function Search_Sorted_Integer_List (List_A, List_B : NL_Types.Integer_List)
                                        return NL_Types.Integer_List;
   function Search_Sorted_Float_List (List_A, List_B : NL_Types.Float_List)
                                        return NL_Types.Integer_List;
   function Set_Diff (Values : Integer_Array; Uniques : Integer_Array)
                      return NL_Types.Natural_List;
   function Set_Diff (Values : Integer_Array; Uniques : Natural_Array)
                      return NL_Types.Natural_List;
   function Set_Diff (Values : Natural_Array; Uniques : Integer_Array)
                      return NL_Types.Natural_List;
   function Set_Diff (Values, Uniques : NL_Arrays_And_Matrices.Boolean_Array)
                      return NL_Types.Boolean_List;
   function Sum_Cols (aList : NL_Types.Float_List_2D) return NL_Types.Float_List;
   function Sum_Cols (aMatrix : Real_Float_Matrix) return Real_Float_Vector;
   function Sum_Cols (aList : ML_Types.Value_Data_Lists_2D)
                      return ML_Types.Value_Data_List;
   function To_Float_List (F : ML_Types.Value_Data_List)
                           return NL_Types.Float_List;
   function To_Float_List (I : NL_Types.Integer_List)
                           return NL_Types.Float_List;
   function To_Float_List_2D (Data : ML_Types.Value_Data_Lists_2D)
                              return NL_Types.Float_List_2D;
   function To_Float_List_2D (I : NL_Types.Integer_List_2D)
                              return NL_Types.Float_List_2D;
   function To_Integer_List (Ints : Integer_Array) return NL_Types.Integer_List;
   function To_Integer_List (Ints : ML_Types.Value_Data_List)
                             return NL_Types.Integer_List;
   function To_Integer_List_2D (Data : ML_Types.Value_Data_Lists_2D)
                                return NL_Types.Integer_List_2D;
   function To_Multi_Value_List (A : Multi_Value_Array)
                                 return ML_Types.Value_Data_Lists_2D;
   function To_Natural_List (A : Natural_Array) return NL_Types.Natural_List;
   function To_Natural_List (Numbers : ML_Types.Value_Data_List)
                             return NL_Types.Natural_List;
   function To_Integer_Value_List (A : Integer_Array)
                                   return ML_Types.Value_Data_List;
   function To_Integer_Value_List_2D (A : Integer_Array)
                                      return ML_Types.Value_Data_Lists_2D;
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
   function Unique (Nums : NL_Types.Integer_List) return NL_Types.Integer_List;

end Classifier_Utilities;
