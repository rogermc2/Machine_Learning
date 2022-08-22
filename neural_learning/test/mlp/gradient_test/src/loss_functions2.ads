
--  Based on scikit-learn/sklearn/neural_network/tests/test_mlp.py test_gradient

with Multilayer_Perceptron; use Multilayer_Perceptron;
with NL_Arrays_And_Matrices; use NL_Arrays_And_Matrices;
with Stochastic_Optimizers; use Stochastic_Optimizers;

package Loss_Functions2 is

    type Loss_Grad_Array is array (Integer range <>) of Loss_Grad_Result;

    function Loss_Grad_Function
      (Self        : MLP_Classifier; Theta : Parameters_List;
       X           : Real_Float_Matrix; Y : Binary_Matrix;
       Gradients : Parameters_List) return Loss_Grad_Result;
    function Numerical_Loss_Grad
      (MLP    : MLP_Classifier; Theta : Parameters_List;
       X      : Real_Float_Matrix; Y : Binary_Matrix;
       Params : Parameters_List) return Real_Float_Vector;

end Loss_Functions2;
