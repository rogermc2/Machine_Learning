
--  Based on scikit-learn/sklearn/neural_network/tests/test_mlp.py test_gradient

with Multilayer_Perceptron; use Multilayer_Perceptron;
with NL_Arrays_And_Matrices; use NL_Arrays_And_Matrices;
with Stochastic_Optimizers; use Stochastic_Optimizers;

package Loss_Functions is

   type Loss_Grad_Array is array (Integer range <>) of Loss_Grad_Result;

   Eps  : constant Float := 10.0 ** (-5);

   function Loss_Grad_Function
     (Self        : in out MLP_Classifier; Theta : Parameters_List;
      X           : Real_Float_Matrix; Y : Boolean_Matrix;
      Activations : Real_Matrix_List; Gradients : Parameters_List)
      return Loss_Grad_Result;
   function Numerical_Loss_Grad
     (aClassifier : in out MLP_Classifier; Theta : Parameters_List;
      X           : Real_Float_Matrix; Y_Bin : Boolean_Matrix;
      Activations : Real_Matrix_List; Params : Parameters_List)
      return Loss_Grad_Array;

end Loss_Functions;
