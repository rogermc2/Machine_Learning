
with NL_Arrays_And_Matrices; use NL_Arrays_And_Matrices;

package Multiclass_Utils is
   type Y_Type is (Y_Unknown, Y_Continuous, Y_Continuous_Multioutput, Y_Binary,
                   Y_Multiclass, Y_Multiclass_Multioutput,
                   Y_Mutilabel_Indicator);
   type Unique_Label is (Unique_Binary, Unique_Multiclass,
                          Unique_Mutilabel_Indicator);

   function Type_Of_Target (Y : Integer_Matrix) return Y_Type;
   function Unique_Labels (Y : Integer_Matrix) return Y_Type;

end Multiclass_Utils;
