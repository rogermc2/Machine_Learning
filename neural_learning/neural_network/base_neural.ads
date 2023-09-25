--  Based on scikit-learn/sklearn/neural_network/_base.py

with ML_Arrays_And_Matrices; use ML_Arrays_And_Matrices;

package Base_Neural is

   type Activation_Type is (Identity_Activation, Logistic_Activation,
                            Tanh_Activation, Rect_LU_Activation,
                            Softmax_Activation);

   type Derivative_Type is (Identity_Derivative, Logistic_Derivative,
                            Tanh_Derivative, Rect_LU_Derivative);

   function Binary_Log_Loss (Y_True, Y_Prob : Real_Float_Matrix) return Float;
   pragma Inline (Binary_Log_Loss);
   procedure Clip (Mat : in out Real_Float_Matrix);
   pragma Inline (Clip);
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
   function Log_Loss (Y_True, Y_Prob : Real_Float_Matrix) return Float;
   --     function Log_Loss (Y_True : Integer_Matrix; Y_Prob : Real_Float_Matrix)
   --                        return Float;
   pragma Inline (Log_Loss);
   procedure Tanh (Activation : in out Real_Float_Matrix);
   pragma Inline (Tanh);
   procedure Tanh_Derivative (Z   : Real_Float_Matrix;
                              Del : in out Real_Float_Matrix);
   pragma Inline (Tanh_Derivative);
   procedure Rect_LU (Activation : in out Float);
   function Rect_LU (Activation : Float) return Float;
   procedure Rect_LU (Activation : in out Real_Float_Matrix);
   procedure Rect_LU (Activation : in out Real_Float_Vector);
   function Rect_LU (Activation : Real_Float_Vector)
                     return Real_Float_Vector;
   pragma Inline (Rect_LU);
   procedure Rect_LU_Derivative (Z   : Real_Float_Matrix;
                                 Del : in out Real_Float_Matrix);
   procedure Rect_LU_Derivative (Z   : Real_Float_Vector;
                                 Del : in out Real_Float_Vector);
   function Rect_LU_Derivative (Z : Real_Float_Vector)
                                return Real_Float_Vector;
   pragma Inline (Rect_LU_Derivative);
   function Sigmoid (X : Float) return Float;
   pragma Inline (Sigmoid);
   procedure Softmax (Activation : in out Float);
   procedure Softmax (Activation : in out Real_Float_Matrix);
   procedure Softmax (Activation : in out Real_Float_Vector);
   pragma Inline (Softmax);
   function Mean_Squared_Error (Y_True : Integer_Matrix;
                               Y_Pred : Real_Float_Matrix) return Float;
   function Mean_Squared_Error (Y_True, Y_Pred : Real_Float_Matrix) return Float;
   function Mean_Squared_Error (Y_True, Y_Pred : Real_Float_Vector)
                               return Float;
   pragma Inline (Mean_Squared_Error);
   function MSE_Derivative (Y_True, Y_Pred : Real_Float_Matrix)
                                     return Real_Float_Matrix;
   function MSE_Derivative (Y_True, Y_Pred : Real_Float_Vector)
                                     return Real_Float_Vector;
   pragma Inline (MSE_Derivative);
   function X_Log_Y (X, Y : Real_Float_Matrix) return Real_Float_Matrix;
   pragma Inline (X_Log_Y);

end Base_Neural;
