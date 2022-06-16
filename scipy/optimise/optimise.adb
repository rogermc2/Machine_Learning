--  Based on scipy/optimize/optimize.py

package body Optimise is

    function Prepare_Scalar_Function
      return Differentiable_Functions.Scalar_Function is
        SF : Differentiable_Functions.Scalar_Function (1);
    begin
        return SF;
    end Prepare_Scalar_Function;

    --  ------------------------------------------------------------------------

    function F_Min_BFGS (X : Real_Float_Arrays.Real_Vector)
                         return Optimise_Result is
        Min_BFGS : Optimise_Result (0, 0, 0);
    begin
        return Min_BFGS;
    end F_Min_BFGS;

    --  ------------------------------------------------------------------------

end Optimise;
