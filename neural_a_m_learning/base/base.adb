--  Based on scikit-learn/sklearn/_base.py

package body Base is

   function Score (X : Float_Matrix; Y : Integer_Matrix;
                   Sample_Weight      : Weights.Weight_List :=
                     NL_Types.Float_Package.Empty_Vector) return Float is
   begin
      return 0.0;
   end Score;

end Base;
