--  Based on scipy/_minimize.py

with Optimise;

package Minimise is

   type Method_Type is (L_BFGS_B_Method);

   function Minimise (Meth : Method_Type) return Optimise.Optimise_Result;

end Minimise;
