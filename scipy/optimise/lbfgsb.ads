--  Based on scipy/optimize/_lbfgsb_py.py

with NL_Arrays_And_Matrices; use NL_Arrays_And_Matrices;
with Optimise;

package LBFGSB is

   function Minimise_LBFGSB ( X : Real_Float_Matrix;
                              Bounds : Real_Float_Matrix)
                             return Optimise.Optimise_Result;

end LBFGSB;
