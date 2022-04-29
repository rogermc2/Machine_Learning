
package body Bayesian is

   function Estimate_Log_Weights (Mixture : in out Bayesian_Gaussian_Mixture;
                                  X       : Float_List_2D;
                                  Y       : Integer_List) return Float is
      Result : Float := 0.0;
   begin
      if Mixture.Parameters.Weight_Concentration_Prior_Kind = Dirichlet_Process then
         null;
      else
         null;
      end if;

      return Result;

   end Estimate_Log_Weights;

end Bayesian;
