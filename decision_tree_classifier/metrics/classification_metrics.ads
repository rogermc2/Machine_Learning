
with Classifier_Types;
with ML_Types;
with Weights;

package Classification_Metrics is

   function Accuracy_Score
     (Actual, Prediction : ML_Types.Value_Data_Lists_2D;
      Normalize          : Boolean := True;
      Sample_Weight      : Weights.Weight_List :=
        Classifier_Types.Float_Package.Empty_Vector)
      return float;

end Classification_Metrics;
