
with PLplot_Auxiliary;

with NL_Arrays_And_Matrices;
with NL_Types; use NL_Types;
with Weights;

package Classifier_Utilities is

   type Feature_Type_Array is array (Positive range <>) of Data_Type;
   type Label_Type_Array is array (Positive range <>) of Data_Type;

   Value_Error : exception;

   function Arg_Max (Values : Weights.Weight_List) return Positive;
   function Arg_Max (Values_2D : Weights.Weight_Lists_2D; Axis : Natural := 0)
                     return Natural_List;
   function Arg_Max (Values_3D : Weights.Weight_Lists_3D; Axis : Natural := 0)
                     return Natural_List;
   function Bin_Count (Numbers : Natural_List) return Natural_List;
   function Bin_Count (Numbers : Value_Data_List) return Natural_List;
   function Compare_Float_Lists (L, R : Float_List) return Boolean;
   function Dot (L : Weights.Weight_List; R : Natural_List) return Float;
   function Get_Column (List_2D      : Float_List_2D;
                        Column_Index : Positive) return Float_List;
   function Float_Precision (Number : Float; Precision : Natural) return String;
   function Init_Samples_Copy (Samples : Value_Data_Lists_2D)
                               return Value_Data_Lists_2D;
   function Load_Data (File_Name : String; Num_Outputs : Positive := 1)
                       return Multi_Output_Data_Record;
   function Samples_3D_To_Outputs_3D (Samples     : Weights.Weight_Lists_3D;
                                      Num_Outputs : Positive)
                                      return Weights.Weight_Lists_3D;
   function Ones (List_Length : Positive) return Weights.Weight_List;
   function Search_Sorted_Integer_List (List_A, List_B : Integer_List)
                                        return Integer_List;
   function Set_Diff (Values : Integer_List; Uniques : Natural_List)
                      return Natural_List;
   function Set_Diff (Values, Uniques : Natural_List) return Natural_List;
   function Set_Value (List_Length : Positive; Value : Float)
                       return Weights.Weight_List;
   function Sum_Cols (aList : Float_List_2D) return Float_List;
   function Sum_Cols (aList : Value_Data_Lists_2D) return Value_Data_List;
   function Sum_Cols (aList : Weights.Weight_Lists_3D)
                      return Weights.Weight_List;
   function To_Float_List (F : Value_Data_List) return Float_List;
   function To_Float_List (I : Integer_List) return Float_List;
   function To_Float_List (A : NL_Arrays_And_Matrices.Float_Array)
                           return NL_Types.Float_List;
   function To_Natural_List (A : NL_Arrays_And_Matrices.Natural_Array)
                             return NL_Types.Natural_List;
   function To_Float_List_2D (I : Integer_List_2D) return Float_List_2D;
   function To_Integer_List (Ints : Value_Data_List) return Integer_List;
   function To_Multi_Value_List (A : NL_Arrays_And_Matrices.Multi_Value_Array)
                                     return Value_Data_Lists_2D;
   function To_Natural_List (Numbers : Value_Data_List) return Natural_List;
   function To_Integer_Value_List (A : NL_Arrays_And_Matrices.Integer_Array)
                                   return Value_Data_List;
   function To_Integer_Value_List_2D (A : NL_Arrays_And_Matrices.Integer_Array)
                                      return Value_Data_Lists_2D;
   function To_Natural_Value_List (A : NL_Arrays_And_Matrices.Natural_Array)
                                   return Value_Data_Lists_2D;
   function To_PL_Array (List_1D  : Float_List; Num_Rows : Positive)
                         return PLplot_Auxiliary.Real_Matrix;
   function To_Value_2D_List (A : Value_Data_List)
                              return Value_Data_Lists_2D;
   function To_Value_2D_List (List_1D  : Value_Data_List;
                              Num_Rows : Positive)
                              return Value_Data_Lists_2D;
   function Transpose (Values : Value_Data_Lists_2D)
                       return  Value_Data_Lists_2D;
   function Unique (Nums : Integer_List) return Integer_List;
   --      function Unique_Integer_Array (Nums : Integer_Array) return Integer_Array;
   function Unique_Weights (Values : Weights.Weight_Lists_2D)
                            return Weights.Weight_List;

end Classifier_Utilities;
