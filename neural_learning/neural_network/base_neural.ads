--  Based on scikit-learn/sklearn/neural_network/_base.py

with NL_Types; use NL_Types;

package Base_Neural is

   type Activation_Type is (Identity_Activation, Logistic_Activation,
                            Tanh_Activation, Relu_Activation,
                            Softmax_Activation);

   type Derivative_Type is (Identity_Derivative, Logistic_Derivative,
                            Tanh_Derivative, Relu_Derivative);

   function Binary_Log_Loss (Y_True : Integer_List_2D; Y_Prob : Float_List_2D)
                             return Float;
   pragma Inline (Binary_Log_Loss);
   function Identity (Activation : Float_List) return Float_List;
   pragma Inline (Identity);
   procedure Identity_Derivative (Z   : Float_List_2D;
                                  Del : in out Float_List_2D);
   pragma Inline (Identity_Derivative);
   function Logistic (Activation : Float_List_2D) return Float_List_2D;
   pragma Inline (Logistic);
   procedure Logistic_Derivative (Z   : Float_List_2D;
                                  Del : in out Float_List_2D);
   pragma Inline (Logistic_Derivative);
   function Logistic_Sigmoid (X : Float) return Float;
   pragma Inline (Logistic_Sigmoid);
   function Log_Loss (Y_True : Integer_List_2D; Y_Prob : Float_List_2D) return Float;
   pragma Inline (Log_Loss);
   function Tanh (Activation : Float_List_2D) return Float_List_2D;
   pragma Inline (Tanh);
   procedure Tanh_Derivative (Z : Float_List_2D; Del : in out Float_List_2D);
   pragma Inline (Tanh_Derivative);
   function Relu (Activation : Float_List_2D) return Float_List_2D;
   pragma Inline (Relu);
   procedure Relu_Derivative (Z : Float_List_2D; Del : in out Float_List_2D);
   pragma Inline (Relu_Derivative);
   function Softmax (Activation : Float_List_2D) return Float_List_2D;
   pragma Inline (Softmax);
   function Squared_Loss (Y_True : Integer_List_2D; Y_Pred : Float_List_2D)
                           return Float;
   pragma Inline (Squared_Loss);

end Base_Neural;
