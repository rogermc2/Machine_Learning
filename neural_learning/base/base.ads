--  Based on scikit-learn/sklearn/base.py

with Multilayer_Perceptron;
with ML_Arrays_And_Matrices; use ML_Arrays_And_Matrices;

package Base is

   function Score (Self : Multilayer_Perceptron.MLP_Classifier;
                   X : Real_Float_Matrix; Y : Integer_Matrix) return Float;
   function Score (Self          : Multilayer_Perceptron.MLP_Classifier;
                   X             : Real_Float_Matrix; Y : Integer_Matrix;
                   Sample_Weight : Real_Float_Vector) return Float;

end Base;
