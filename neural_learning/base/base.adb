--  Based on scikit-learn/sklearn/base.py

--  with Ada.Text_IO; use Ada.Text_IO;

with Classification_Metrics;

package body Base is
   --  Score returns the mean accuracy on the given test data and labels.
   function Score (Self : Multilayer_Perceptron.MLP_Classifier;
                   X, Y : Real_Float_Matrix; Sample_Weight : Real_Float_Vector)
                   return Float is
   begin
      return Classification_Metrics.Accuracy_Score
        (Y_True  => Y, Y_Prediction  => Multilayer_Perceptron.Predict (Self, X),
         Normalize => True, Sample_Weight => Sample_Weight);
   end Score;

end Base;
