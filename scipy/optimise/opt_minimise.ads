--  Based on scipy/optimise/_minimize.py

with Constraints;
with Multilayer_Perceptron;
with Optimise;
with Stochastic_Optimizers;

package Opt_Minimise is

   type Method_Type is (L_BFGS_B_Method);

   function Minimise (Fun  : Multilayer_Perceptron.Max_Function_Access;
                      X0   : Stochastic_Optimizers.Parameters_List;
                      Meth : Method_Type; Bounds : Constraints.Array_Bounds)
                      return Optimise.Optimise_Result;

end Opt_Minimise;
