--  Based on scipy/optimise/_minimize.py

with LBFGSB;

package body Opt_Minimise is

    function Minimise (Fun  : Multilayer_Perceptron.Max_Function_Access;
                       X0   : Stochastic_Optimizers.Parameters_List;
                       Meth   : Method_Type := No_Method; Jac : Boolean := False;
                       Bounds : Constraints.Bounds_List :=
                         Constraints.Array_Bounds_Package.Empty_Vector;
                       Constraints : Minimise_Constraints_List :=
                         Minimise_Constraints_Package.Empty_List;
                       Options : Minimise_Options := No_Options)
                       return Optimise.Optimise_Result is
    --          use Optimise;
        Method   : Method_Type := Meth;
        --          Result : Optimise_Result;
    begin
        --  L531
        if Meth = No_Method then
            if not Constraints.Is_Empty then
                Method := Slsqp_Method;
            elsif not Bounds.Is_Empty then
                Method := L_BFGS_B_Method;
            else
                Method := BFGS_Method;
            end if;
        end if;

        return LBFGSB.Minimise_LBFGSB (Fun, X0, Meth, Bounds);

    end Minimise;

end Opt_Minimise;
