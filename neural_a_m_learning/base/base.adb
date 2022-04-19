--  Based on scikit-learn/sklearn/base.py

with Classification_Metrics;

package body Base is
   --  Score returns the mean accuracy on the given test data and labels.
   function Score (X : Float_Matrix; Y : Integer_Matrix;
                   Sample_Weight      : NL_Types.Float_List :=
                     NL_Types.Float_Package.Empty_Vector) return Float is
   begin
      return Classification_Metrics.Accuracy_Score (Y, Predict (X),
                                                    Sample_Weight);
   end Score;

end Base;
