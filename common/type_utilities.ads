
with ML_Arrays_And_Matrices; use ML_Arrays_And_Matrices;
with ML_Types;
with NL_Types;

package Type_Utilities is

   type Feature_Type_Array is array (Positive range <>) of ML_Types.Data_Type;
   type Label_Type_Array is array (Positive range <>) of ML_Types.Data_Type;

   Value_Error : exception;

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

end Type_Utilities;
