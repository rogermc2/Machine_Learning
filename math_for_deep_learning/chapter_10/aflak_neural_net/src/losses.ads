
with ML_Arrays_And_Matrices; use ML_Arrays_And_Matrices;

package Losses is

   function Binary_Cross_Entropy (Y_True, Y_Pred : Binary_Array)
                                  return Float;
   function Binary_Cross_Entropy_Prime (Y_True, Y_Pred : Binary_Array)
                                        return Real_Float_Vector;
   function MSE (Y_True,Y_Pred : Binary_Array) return Float;
   function MSE_Prime (Y_True, Y_Pred : Binary_Array)
                       return Real_Float_Vector;
end Losses;
