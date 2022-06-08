--  Based on scipy/optimize/lbfgsb_py.py

with NL_Arrays_And_Matrices; use NL_Arrays_And_Matrices;

with Constraints;
with Multilayer_Perceptron;
with Stochastic_Optimizers;
with Optimise;
with Opt_Minimise;

package LBFGSB is

    function Minimise_LBFGSB (Fun    : Multilayer_Perceptron.Max_Function_Access;
                              X0     : Real_Float_Matrix;
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
