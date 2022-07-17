--  Based on scikit-learn/sklearn/neural_network/_base.py

with NL_Arrays_And_Matrices; use NL_Arrays_And_Matrices;

package Base_Neural is

   type Activation_Type is (Identity_Activation, Logistic_Activation,
                            Tanh_Activation, Rect_LU_Activation,
                            Softmax_Activation);

   type Derivative_Type is (Identity_Derivative, Logistic_Derivative,
                            Tanh_Derivative, Rect_LU_Derivative);

   function Binary_Log_Loss (Y_True : Boolean_Matrix;
                             Y_Prob : Real_Float_Matrix) return Float;
   function Binary_Log_Loss (Y_True : Integer_Matrix;
                             Y_Prob : Real_Float_Matrix) return Float;
   pragma Inline (Binary_Log_Loss);
   procedure Identity (Activation : Real_Float_Matrix);
   pragma Inline (Identity);
   procedure Identity_Derivative (Z   : Real_Float_Matrix;
                                  Del : in out Real_Float_Matrix);
   pragma Inline (Identity_Derivative);
   procedure Logistic (Activation : in out Real_Float_Matrix);
   pragma Inline (Logistic);
   procedure Logistic_Derivative (Z   : Real_Float_Matrix;
                                  Del : in out Real_Float_Matrix);
   pragma Inline (Logistic_Derivative);
   function Logistic_Sigmoid (X : Long_Float) return Float;
   function Logistic_Sigmoid (X : Real_Float_Matrix)
                               return Real_Float_Matrix;
   pragma Inline (Logistic_Sigmoid);
   function Log_Loss (Y_True : Boolean_Matrix; Y_Prob : Real_Float_Matrix)
                      return Float;
   function Log_Loss (Y_True : Integer_Matrix; Y_Prob : Real_Float_Matrix)
                      return Float;
   pragma Inline (Log_Loss);
   procedure Tanh (Activation : in out Real_Float_Matrix);
   pragma Inline (Tanh);
   procedure Tanh_Derivative (Z   : Real_Float_Matrix;
                              Del : in out Real_Float_Matrix);
   pragma Inline (Tanh_Derivative);
   procedure Rect_LU (Activation : in out Real_Float_Matrix);
   pragma Inline (Rect_LU);
   procedure Rect_LU_Derivative (Z : Real_Float_Matrix;
                              Del : in out Real_Float_Matrix);
   pragma Inline (Rect_LU_Derivative);
   procedure Softmax (Activation : in out Real_Float_Matrix);
   pragma Inline (Softmax);
   function Squared_Loss (Y_True : Integer_Matrix; Y_Pred : Real_Float_Matrix)
                           return Float;
   function Squared_Loss (Y_True : Boolean_Matrix; Y_Pred : Real_Float_Matrix)
                           return Float;
   pragma Inline (Squared_Loss);

end Base_Neural;
