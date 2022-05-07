--  Based on scikit-learn/sklearn/metrics/_classification.py

with NL_Arrays_And_Matrices; use NL_Arrays_And_Matrices;

package Classification_Metrics is

   function Accuracy_Score
     (Y_True, Y_Prediction : Real_Float_Matrix; Normalize : Boolean := True;
      Sample_Weight : Real_Float_Vector) return Float;

end Classification_Metrics;
