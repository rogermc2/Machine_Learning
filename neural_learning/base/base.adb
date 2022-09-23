--  Based on scikit-learn/sklearn/base.py

with Ada.Text_IO; use Ada.Text_IO;

with Classification_Metrics;
--  with Printing;
with Test_Support;

package body Base is

   --  Score returns the mean accuracy on the given test data and labels.
   function Score (Self : Multilayer_Perceptron.MLP_Classifier;
                   X    : Real_Float_Matrix; Y : Integer_Matrix)
                   return Float is
      Routine_Name : constant String := "Base.Score ";
      --  Pred returns a matrix of samples x classes
      Pred         : constant Integer_Matrix :=
                       Multilayer_Perceptron.Predict (Self, X);
   begin
      NL_Arrays_And_Matrices.Check_Lengths (Routine_Name, Y,  Pred);
      Test_Support.Print_Integer_Matrix (Routine_Name & "Y", Y, 1, 10);
      Test_Support.Print_Integer_Matrix (Routine_Name & "Pred", Pred, 1, 10);
      return Classification_Metrics.Accuracy_Score
        (Y_True  => Y, Y_Prediction => Pred, Normalize => True);
   end Score;

   --  ------------------------------------------------------------------------

   function Score (Self          : Multilayer_Perceptron.MLP_Classifier;
                   X             : Real_Float_Matrix; Y : Integer_Matrix;
                   Sample_Weight : Real_Float_Vector) return Float is
      Routine_Name : constant String := "Base.Score with Weight ";
      Pred         : constant Integer_Matrix :=
                       Multilayer_Perceptron.Predict (Self, X);
   begin
      Put_Line (Routine_Name & "Y'Length (2)" & Integer'Image (Y'Length (2)));
      Check_Lengths (Routine_Name, Pred, Y);
      return Classification_Metrics.Accuracy_Score
        (Y_True  => Y, Y_Prediction => Pred, Normalize => True,
         Sample_Weight => Sample_Weight);
   end Score;

end Base;
