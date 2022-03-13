--  Based on scikit-learn/sklearn/neural_network/_base.py

with IL_Types; use IL_Types;

package Base is

   type Activation_Type is (Identity_Activation, Logistic_Activation,
                            Tanh_Activation, Relu_Activation,
                            Softmax_Activation);

   function Identity (Activation : Float_List) return Float_List;
   pragma Inline (Identity);
   function Logistic (Activation : Float_List) return Float_List;
   pragma Inline (Logistic);
   function Logistic_Sigmoid (X : Float) return Float;
   pragma Inline (Logistic_Sigmoid);
   function Tanh (Activation : Float_List) return Float_List;
   pragma Inline (Tanh);
   function Relu (Activation : Float_List) return Float_List;
   pragma Inline (Relu);
   function Softmax (Activation : Float_List) return Float_List;
   pragma Inline (Softmax);

end Base;
