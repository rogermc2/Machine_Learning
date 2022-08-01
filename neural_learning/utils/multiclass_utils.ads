--  Based on scikit-learn/sklearn/utils.multiclass.py

with NL_Types;
with NL_Arrays_And_Matrices; use NL_Arrays_And_Matrices;

package Multiclass_Utils is
   type Y_Type is (Y_Unknown, Y_Continuous, Y_Continuous_Multioutput, Y_Binary,
                   Y_Multiclass, Y_Multiclass_Multioutput,
                   Y_Multilabel_Indicator);
   type Label_Type is (Unique_Binary, Unique_Multiclass,
                       Unique_Mutilabel_Indicator);

   function Is_Multilabel (Y : Real_Float_Matrix) return Boolean;
   function Type_Of_Target (Y : Binary_Matrix) return Y_Type;
   function Type_Of_Target (Y : Boolean_Matrix) return Y_Type;
   function Type_Of_Target (Y : Integer_Array) return Y_Type;
   function Type_Of_Target (Y : NL_Types.Integer_List) return Y_Type;
   function Type_Of_Target (Y : NL_Types.Array_Of_Integer_Lists) return Y_Type;
   function Type_Of_Target (Y : Integer_Matrix) return Y_Type;
   function Type_Of_Target (Y : Real_Float_Matrix) return Y_Type;
   function Unique_Labels (Y : Binary_Matrix) return NL_Types.Integer_List;
   function Unique_Labels (Y : Integer_Array) return NL_Types.Integer_List;
   function Unique_Labels (Y : NL_Types.Integer_List)
                           return NL_Types.Integer_List;
   function Unique_Labels (Y : Integer_Matrix) return NL_Types.Integer_List;
   function Unique_Labels (Y : NL_Types.Array_Of_Integer_Lists)
                            return NL_Types.Integer_List;
   function Unique_Labels (Y : Real_Float_Matrix) return NL_Types.Float_List;

end Multiclass_Utils;
