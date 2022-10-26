--  Based on scipy/_minimize.py

package body Minimise is

   --  Minimization of a scalar function of one or more variables.

   function Minimise (Meth : Method_Type) return Optimise.Optimise_Result is
      Result : Optimise.Optimise_Result;
   begin
      if Meth = L_BFGS_B_Method then
         null;
      end if;

      return Result;

   end Minimise;

end Minimise;
