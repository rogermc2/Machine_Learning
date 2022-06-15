--  Based on scipy/optimize/lbfgsb_py.py

with Constraints;
with Stochastic_Optimizers;
with Optimise;
with Opt_Minimise;

package LBFGSB is

    function Minimise_LBFGSB (Fun    : Optimise.Opt_Fun_Access;
                              X0     : Stochastic_Optimizers.Parameters_List;
                              Meth   : Opt_Minimise.Method_Type;
                              Bounds : Constraints.Bounds_List :=
                                Constraints.Array_Bounds_Package.Empty_Vector;
                              Max_Cor : Positive := 10;
                              Ftol    : Float :=
                                2.2204460492503131 * 10.0 ** (-09);
                              Gtol     : Float := 10.0 ** (-5);
                              Eps      : Float := 10.0 ** (-8);
                              Max_Fun  : Positive := 15000;
                              Max_Iter  : Positive := 15000;
                              Options   : Opt_Minimise.Minimise_Options :=
                                Opt_Minimise.No_Options)
                             return Optimise.Optimise_Result;

end LBFGSB;
