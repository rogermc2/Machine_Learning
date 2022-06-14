--  Based on scipy/optimise/_minimize.py

with Constraints;
with Multilayer_Perceptron;
with Optimise;
with Stochastic_Optimizers;

package Opt_Minimise is

   type Method_Type is (BFGS_Method, L_BFGS_B_Method, Nelder_Mead_Method,
                        Powell_Method, Cobyla_Method, Cg_Method, N_Cg_Method,
                        Newton_Cg_Method, Dogleg_Method, Trust_Ncg_Method,
                        Tnc_Method, Trust_Constr_Method, Trust_Krylov_Method,
                        Trust_Exact_Method, Slsqp_Method, Custom_Method);
   type Minimise_Options is record
      Max_Fun        : Multilayer_Perceptron.Max_Function_Access;
      Max_Iter       : Positive;
      G_Tolerance    : Float;
      Max_Line_Steps : Natural:= 20;
   end record;

   No_Options : constant Minimise_Options := (Null, 1, 0.0, 0);

   function Minimise (Fun  : Multilayer_Perceptron.Max_Function_Access;
                      X0   : Stochastic_Optimizers.Parameters_List;
                      Meth   : Method_Type; Jac : Boolean := False;
                      Bounds : Constraints.Bounds_List :=
                        Constraints.Array_Bounds_Package.Empty_Vector;
                      Options : Minimise_Options := No_Options)
                      return Optimise.Optimise_Result;

end Opt_Minimise;
