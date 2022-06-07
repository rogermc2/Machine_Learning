--  Based on scipy/optimize/lbfgsb_py.py

--  with NL_Arrays_And_Matrices; use NL_Arrays_And_Matrices;

with Constraints;
with Multilayer_Perceptron;
with Stochastic_Optimizers;
with Optimise;
with Opt_Minimise;

package LBFGSB is

   function Minimise_LBFGSB (Fun  : Multilayer_Perceptron.Max_Function_Access;
                             X : Stochastic_Optimizers.Parameters_List;
                             Meth : Opt_Minimise.Method_Type;
                             Bounds : Constraints.Array_Bounds)
                             return Optimise.Optimise_Result;

end LBFGSB;
