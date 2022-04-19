--  Based on scikit-learn/sklearn/base.py

with NL_Arrays_And_Matrices; use NL_Arrays_And_Matrices;
with NL_Types;

package Base is

   function Score (X                  : Float_Matrix; Y : Integer_Matrix;
                   Sample_Weight      : NL_Types.Float_List :=
                     NL_Types.Float_Package.Empty_Vector) return Float;

end Base;
