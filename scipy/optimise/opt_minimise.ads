--  Based on scipy/optimise/_minimize.py

with Ada.Containers.Doubly_Linked_Lists;

with Constraints;
with Differentiable_Functions;
with Multilayer_Perceptron;
with Num_Diff;
with Optimise;
with Stochastic_Optimizers;

package Opt_Minimise is

   type Minimise_Constraints_Type is
     (No_Constraint, Linear_Constraint, Nonlinear_Constraint,
      Callable_Constraint);

   package Minimise_Constraints_Package is new
     Ada.Containers.Doubly_Linked_Lists (Minimise_Constraints_Type);
   subtype Minimise_Constraints_List is Minimise_Constraints_Package.List;

   type Method_Type is
     (No_Method, New_Method, Old_Method, BFGS_Method, L_BFGS_B_Method,
       Nelder_Mead_Method,Powell_Method, Cobyla_Method, Cg_Method, N_Cg_Method,
      Tnc_Method, Newton_Cg_Method, Dogleg_Method, Trust_Ncg_Method,
      Trust_Krylov_Method, Trust_Constr_Method, Trust_Exact_Method,
      Slsqp_Method, Custom_Method);

   type Minimise_Options is record
      Max_Fun        : Multilayer_Perceptron.Max_Function_Access;
      Max_Iter       : Positive;
      G_Tolerance    : Float;
      Max_Line_Steps : Natural:= 20;
   end record;

   No_Options : constant Minimise_Options := (Null, 1, 0.0, 0);

   function Minimise (Fun         : Differentiable_Functions.RF_Fun_Access;
                      X0          : Stochastic_Optimizers.Parameters_List;
                      Method      : Method_Type := No_Method;
                      Jac         : Num_Diff.FD_Methods := Num_Diff.FD_None;
                      Bounds      : Constraints.Bounds_List :=
                        Constraints.Array_Bounds_Package.Empty_Vector;
                      Constraints : Minimise_Constraints_List :=
                        Minimise_Constraints_Package.Empty_List)
--                        Options     : Minimise_Options := No_Options)
                      return Optimise.Optimise_Result;

end Opt_Minimise;
