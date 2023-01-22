--  Based on scikit-learn/sklearn/utils.multiclass.py

with ML_Types;
with NL_Types;
with ML_Arrays_And_Matrices; use ML_Arrays_And_Matrices;

package Multiclass_Utils is
   type Y_Type is (Y_Unknown, Y_Continuous, Y_Continuous_Multioutput, Y_Binary,
                   Y_Multiclass, Y_Multiclass_Multioutput,
                   Y_Multilabel_Indicator, Y_Multilabel_Sequences);
   type Label_Type is (Unique_Binary, Unique_Multiclass,
                       Unique_Mutilabel_Indicator);

   function Check_Classification_Targets (Y : Binary_Matrix) return Boolean;
   function Is_Multilabel (Y : Real_Float_Matrix) return Boolean;
   function Type_Of_Target (Y : Binary_Array) return Y_Type;
   function Type_Of_Target (Y : Binary_Matrix) return Y_Type;
   function Type_Of_Target (Y : Boolean_Matrix) return Y_Type;
   function Type_Of_Target (Y : Integer_Array) return Y_Type;
   function Type_Of_Target (Y : ML_Types.Integer_List) return Y_Type;
   function Type_Of_Target (Y : ML_Types.Array_Of_Integer_Lists) return Y_Type;
   function Type_Of_Target (Y : Integer_Matrix) return Y_Type;
   function Type_Of_Target (Y : String_Array) return Y_Type;
   function Type_Of_Target (Y : String_Matrix) return Y_Type;
   function Type_Of_Target (Y : Real_Float_Vector) return Y_Type;
   function Type_Of_Target (Y : Real_Float_Matrix) return Y_Type;
   function Type_Of_Target (Y : Unbounded_String_Array) return Y_Type;
   function Type_Of_Target (Y : Unbounded_String_Matrix) return Y_Type;
   function Unique_Labels (Y : Binary_Matrix) return ML_Types.Integer_List;
   function Unique_Labels (Y : Integer_Array) return ML_Types.Integer_List;
   function Unique_Labels (Y : Integer_Array_List) return ML_Types.Integer_List;
   function Unique_Labels (Y : Unbounded_String_Array_List)
                           return ML_Types.Unbounded_List;
   function Unique_Labels (Y : ML_Types.Integer_List)
                           return ML_Types.Integer_List;
   function Unique_Labels (Y : Integer_Matrix) return ML_Types.Integer_List;
   function Unique_Labels (Y : ML_Types.Array_Of_Integer_Lists)
                            return ML_Types.Integer_List;
   function Unique_Labels (Y : Real_Float_Matrix) return NL_Types.Float_List;
   function Unique_Labels (Y : Unbounded_String_Array)
                           return ML_Types.Unbounded_List;
   function Unique_Labels (Y : Unbounded_String_Matrix)
                           return ML_Types.Unbounded_List;

end Multiclass_Utils;
