--  Based on scikit-learn/sklearn/base.py

--  with Ada.Text_IO; use Ada.Text_IO;

with Classification_Metrics;
--  with Printing;

package body Base is

   --  Score returns the mean accuracy on the given test data and labels.
   function Score (Self : Multilayer_Perceptron.MLP_Classifier;
                   X : Real_Float_Matrix; Y : Integer_Matrix)
                   return Float is
      Routine_Name : constant String := "Base.Score ";
      Pred         : constant Binary_Matrix :=
                         Multilayer_Perceptron.Predict (Self, X);
   begin
--        Printing.Print_Float_Matrix ("Routine_Name X", X, 1 ,1);
      Check_Lengths (Routine_Name, Pred, Y);
      return Classification_Metrics.Accuracy_Score
        (Y_True  => Y, Y_Prediction => Pred, Normalize => True);
   end Score;

    --  ------------------------------------------------------------------------

   function Score (Self : Multilayer_Perceptron.MLP_Classifier;
                   X : Real_Float_Matrix; Y : Integer_Matrix;
                   Sample_Weight : Real_Float_Vector)
                   return Float is
      Routine_Name : constant String := "Base.Score with Weight ";
      Pred         : constant Binary_Matrix :=
                         Multilayer_Perceptron.Predict (Self, X);
   begin
      Check_Lengths (Routine_Name, Pred, Y);
--        Printing.Print_Float_Matrix ("Routine_Name X", X, 1 ,1);
      return Classification_Metrics.Accuracy_Score
        (Y_True  => Y, Y_Prediction => Pred, Normalize => True,
        Sample_Weight => Sample_Weight);
   end Score;

end Base;
