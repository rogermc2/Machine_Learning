--  Based on scikit-learn/sklearn/metrics/_classification.py

with NL_Arrays_And_Matrices; use NL_Arrays_And_Matrices;
with NL_Types;

package Classification_Metrics is

   function Accuracy_Score
     (Y_True, Y_Prediction : Integer_Matrix;
      Normalize            : Boolean := True;
      Sample_Weight        : NL_Types.Float_List :=
        NL_Types.Float_Package.Empty_Vector)
       return float;

end Classification_Metrics;
