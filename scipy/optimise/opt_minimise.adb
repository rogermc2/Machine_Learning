--  Based on scipy/optimise/_minimize.py

with LBFGSB;

package body Opt_Minimise is

   function Minimise (Fun  : Multilayer_Perceptron.Max_Function_Access;
                      X0   : Stochastic_Optimizers.Parameters_List;
                      Meth   : Method_Type; Jac : Boolean := False;
                      Bounds : Constraints.Bounds_List :=
                                Constraints.Array_Bounds_Package.Empty_Vector;
                      Options : Minimise_Options := No_Options)
                      return Optimise.Optimise_Result is
--          use Optimise;
--          Result : Optimise_Result;
   begin
        return LBFGSB.Minimise_LBFGSB (Fun, X0, Meth, Bounds);

   end Minimise;

end Opt_Minimise;
