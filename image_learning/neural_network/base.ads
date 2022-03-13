--  Based on scikit-learn/sklearn/neural_network/_base.py

with IL_Types; use IL_Types;

package Base is

   type Activation_Type is (Identity_Activation, Logistic_Activation,
                            Tanh_Activation, Relu_Activation,
                            Softmax_Activation);

   function Identity (X : Float_List_2D) return Float_List_2D;
   pragma Inline (Identity);
   function Logistic (X : Float_List_2D) return Float_List_2D;
   pragma Inline (Logistic);
   function Tanh (X : Float_List_2D) return Float_List_2D;
   pragma Inline (Tanh);
   function Relu (X : Float_List_2D) return Float_List_2D;
   pragma Inline (Relu);
   function Softmax (X : Float_List_2D) return Float_List_2D;
   pragma Inline (Softmax);

end Base;
