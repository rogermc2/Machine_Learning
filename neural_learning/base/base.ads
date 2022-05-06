--  Based on scikit-learn/sklearn/base.py

with Multilayer_Perceptron;
with NL_Arrays_And_Matrices; use NL_Arrays_And_Matrices;

package Base is

   function Score (Self          : Multilayer_Perceptron.MLP_Classifier;
                   X, Y          : Long_Float_Matrix;
                   Sample_Weight : Float_Array) return Float;

end Base;
