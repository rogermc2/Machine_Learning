--  Based on scikit-learn/sklearn/neural_network/_base.py

with NL_Arrays_And_Matrices; use NL_Arrays_And_Matrices;

package Base_Neural is

   type Activation_Type is (Identity_Activation, Logistic_Activation,
                            Tanh_Activation, Relu_Activation,
                            Softmax_Activation);

   type Derivative_Type is (Identity_Derivative, Logistic_Derivative,
                            Tanh_Derivative, Relu_Derivative);

   function Binary_Log_Loss (Y_True : Integer_Matrix; Y_Prob : Float_Matrix)
                             return Float;
   pragma Inline (Binary_Log_Loss);
   procedure Identity (Activation : Float_Matrix);
   pragma Inline (Identity);
   procedure Identity_Derivative (Z   : Float_Matrix;
                                  Del : in out Float_Matrix);
   pragma Inline (Identity_Derivative);
   procedure Logistic (Activation : in out Float_Matrix);
   pragma Inline (Logistic);
   procedure Logistic_Derivative (Z   : Float_Matrix;
                                  Del : in out Float_Matrix);
   pragma Inline (Logistic_Derivative);
   function Logistic_Sigmoid (X : Float) return Float;
   pragma Inline (Logistic_Sigmoid);
   function Log_Loss (Y_True : Integer_Matrix; Y_Prob : Float_Matrix)
                      return Float;
   pragma Inline (Log_Loss);
   procedure Tanh (Activation : in out Float_Matrix);
   pragma Inline (Tanh);
   procedure Tanh_Derivative (Z : Float_Matrix; Del : in out Float_Matrix);
   pragma Inline (Tanh_Derivative);
   procedure Relu (Activation : in out Float_Matrix);
   pragma Inline (Relu);
   procedure Relu_Derivative (Z : Float_Matrix; Del : in out Float_Matrix);
   pragma Inline (Relu_Derivative);
   procedure Softmax (Activation : in out Float_Matrix);
   pragma Inline (Softmax);
   function Squared_Loss (Y_True : Integer_Matrix; Y_Pred : Float_Matrix)
                           return Float;
   pragma Inline (Squared_Loss);

end Base_Neural;