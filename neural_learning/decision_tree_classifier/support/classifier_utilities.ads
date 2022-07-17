
with NL_Arrays_And_Matrices; use NL_Arrays_And_Matrices;
with NL_Types; use NL_Types;

package Classifier_Utilities is

   type Feature_Type_Array is array (Positive range <>) of Data_Type;
   type Label_Type_Array is array (Positive range <>) of Data_Type;

   Value_Error : exception;

   function Arg_Max (Values : Boolean_Array) return Positive;
   function Arg_Max (Values : Boolean_List) return Positive;
   function Arg_Max (Values : Float_List) return Positive;
   function Arg_Max (Values : Integer_List) return Positive;
   function Bin_Count (Numbers : Natural_List) return Natural_List;
   function Bin_Count (Numbers : Value_Data_List) return Natural_List;
   function Compare_Float_Lists (L, R : Float_List) return Boolean;
   function Get_Column (List_2D      : Float_List_2D;
                        Column_Index : Positive) return Float_List;
   function Float_Precision (Number : Float; Precision : Natural) return String;
   function Init_Samples_Copy (Samples : Value_Data_Lists_2D)
                               return Value_Data_Lists_2D;
   function Load_Data (File_Name : String; Num_Outputs : Positive := 1)
                       return Multi_Output_Data_Record;
   function Row_Max_Indices (Values : Boolean_Matrix) return Natural_Array;
   function Row_Max_Indices (Values : Real_Float_Matrix) return Natural_Array;
   function Search_Sorted_Integer_List (List_A, List_B : Integer_List)
                                        return Integer_List;
   function Set_Diff (Values : Integer_Array; Uniques : Integer_Array)
                      return Natural_List;
   function Set_Diff (Values : Integer_Array; Uniques : Natural_Array)
                      return Natural_List;
   function Set_Diff (Values : Natural_Array; Uniques : Integer_Array)
                      return Natural_List;
   function Set_Diff (Values, Uniques : Boolean_Array) return Boolean_List;
   function Sum_Cols (aList : Float_List_2D) return Float_List;
   function Sum_Cols (aList : Value_Data_Lists_2D) return Value_Data_List;
   function To_Float_List (F : Value_Data_List) return Float_List;
   function To_Float_List (I : Integer_List) return Float_List;
   function To_Float_List_2D (Data : Value_Data_Lists_2D) return Float_List_2D;
   function To_Float_List_2D (I : Integer_List_2D) return Float_List_2D;
   function To_Integer_List (Ints : Value_Data_List) return Integer_List;
   function To_Integer_List_2D (Data : Value_Data_Lists_2D) return Integer_List_2D;
   function To_Multi_Value_List (A : Multi_Value_Array)
                                 return Value_Data_Lists_2D;
   function To_Natural_List (A : Natural_Array) return NL_Types.Natural_List;
   function To_Natural_List (Numbers : Value_Data_List) return Natural_List;
   function To_Integer_Value_List (A : NL_Arrays_And_Matrices.Integer_Array)
                                   return Value_Data_List;
   function To_Integer_Value_List_2D (A : NL_Arrays_And_Matrices.Integer_Array)
                                      return Value_Data_Lists_2D;
   function To_Natural_Value_List (A : NL_Arrays_And_Matrices.Natural_Array)
                                   return Value_Data_Lists_2D;
   function To_PL_Array (List_1D  : Float_List; Num_Rows : Positive)
                         return Real_Float_Matrix;
   function To_Value_2D_List (A : Value_Data_List)
                              return Value_Data_Lists_2D;
   function To_Value_2D_List (List_1D  : Value_Data_List;
                              Num_Rows : Positive)
                              return Value_Data_Lists_2D;
   function Transpose (Values : Value_Data_Lists_2D)
                       return  Value_Data_Lists_2D;
   function Unique (Nums : Integer_List) return Integer_List;

end Classifier_Utilities;
