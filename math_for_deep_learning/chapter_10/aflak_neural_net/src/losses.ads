
with ML_Arrays_And_Matrices; use ML_Arrays_And_Matrices;

package Losses is

   function Mean_Square_Error (Y_True,Y_Pred : Real_Float_Vector)
                               return Float;
   function Minus_MSE_Derivative (Y_True, Y_Pred : Real_Float_Vector)
                                  return Real_Float_Vector;
end Losses;
