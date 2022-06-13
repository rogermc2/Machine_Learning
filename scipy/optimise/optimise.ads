--  Based on scipy/optimize/optimize.py

with NL_Arrays_And_Matrices; use NL_Arrays_And_Matrices;
with Differentiable_Functions;

package Optimise is

   type Optimise_Result is record
      X       : Real_Float_List;
      Success : Boolean := False;
   end record;

   function Prepare_Scalar_Function
      return Differentiable_Functions.Scalar_Function;
   function F_Min_BFGS ( X : Real_Float_Arrays.Real_Vector)
                        return Optimise_Result;

end Optimise;
