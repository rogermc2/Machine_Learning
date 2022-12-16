--  Based on scipy/optimize/lbfgsb_py.py

with Opt_Constraints;
with Multilayer_Perceptron;
with Optimise;
with Opt_Minimise;
with ML_Arrays_And_Matrices; use ML_Arrays_And_Matrices;
with Stochastic_Optimizers;

package L_BFGS_B is

    function All_Close (A, B  : Real_Float_Vector;
                        A_Tol : Float := 10.0 ** (-8))
                       return Boolean;
    function Minimise_LBFGSB (Fun      : Multilayer_Perceptron.Loss_Grad_Access;
                              Args     : Multilayer_Perceptron.Loss_Grad_Args;
                              X0        : Stochastic_Optimizers.Parameters_List;
                              Bounds    : Opt_Constraints.Bounds_List :=
                                Opt_Constraints.Array_Bounds_Package.Empty_Vector;
                              Max_Cor   : Positive := 10;
                              Ftol      : Float :=
                                2.2204460492503131 * 10.0 ** (-09);
                              Gtol      : Float := 10.0 ** (-5);
                              Eps       : Float := 10.0 ** (-8);
                              Max_Fun   : Positive := 15000;
                              Max_Iter  : Positive := 15000;
                              Options   : Opt_Minimise.Minimise_Options :=
                                Opt_Minimise.No_Options)
                             return Optimise.Optimise_Result;

end L_BFGS_B;
