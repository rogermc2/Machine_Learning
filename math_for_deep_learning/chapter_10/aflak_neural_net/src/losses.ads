
with ML_Arrays_And_Matrices; use ML_Arrays_And_Matrices;

package Losses is

   function Binary_Cross_Entropy (Y_True, Y_Pred : Real_Float_Vector)
                                  return Float;
   function Binary_Cross_Entropy_Prime (Y_True, Y_Pred : Real_Float_Vector)
                                        return Real_Float_Vector;
   function MSE (Y_True,Y_Pred : Real_Float_Vector) return Float;
   function MSE_Prime (Y_True, Y_Pred : Real_Float_Vector)
                       return Real_Float_Vector;
end Losses;
