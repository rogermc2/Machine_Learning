--  Based on scipy/optimise/_minimize.py

with Constraints;
with Multilayer_Perceptron;
with Optimise;
with Stochastic_Optimizers;

package Opt_Minimise is

   type Method_Type is (L_BFGS_B_Method);
   type Minimise_Options is record
      Max_Fun     : Multilayer_Perceptron.Max_Function_Access;
      Max_Iter    : Positive;
      G_Tolerance : Float;
   end record;

   No_Options : constant Minimise_Options := (Null, 1, 0.0);

   function Minimise (Fun  : Multilayer_Perceptron.Max_Function_Access;
                      X0   : Stochastic_Optimizers.Parameters_List;
                      Meth   : Method_Type; Jac : Boolean := False;
                      Bounds : Constraints.Bounds_List :=
                        Constraints.Array_Bounds_Package.Empty_Vector;
                      Options : Minimise_Options := No_Options)
                      return Optimise.Optimise_Result;

end Opt_Minimise;
