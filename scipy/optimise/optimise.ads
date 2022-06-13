--  Based on scipy/optimize/optimize.py

with NL_Arrays_And_Matrices; use NL_Arrays_And_Matrices;
with Differentiable_Functions;

package Optimise is

   type Opt_Fun_Access is access function (X : Real_Float_List) return Float;

   type Optimise_Result is record
      Fun     : Opt_Fun_Access;
      X       : Real_Float_List;
      Nit     : Positive := 1;
      Success : Boolean := False;
   end record;

   function Prepare_Scalar_Function
      return Differentiable_Functions.Scalar_Function;
   function F_Min_BFGS ( X : Real_Float_Arrays.Real_Vector)
                        return Optimise_Result;

end Optimise;
