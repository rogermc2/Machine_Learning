--  Based on scipy/optimize/lbfgsb_py.py

with Interfaces.Fortran; use Interfaces.Fortran;

with Constraints;
with Lbfgsb_F_Interface; use Lbfgsb_F_Interface;
with Optimise;
with Opt_Minimise;
with Num_Diff;
with Stochastic_Optimizers;

package LBFGSB1 is

   function All_Close (A, B  : Fortran_DP_Array;
                       A_Tol : Double_Precision := 10.0 ** (-8))
                       return Boolean;
   function Minimise_LBFGSB (Fun       : Num_Diff.Deriv_Float_Fun_Access;
                             X0        : Stochastic_Optimizers.Parameters_List;
                             Bounds    : Constraints.Bounds_List :=
                               Constraints.Array_Bounds_Package.Empty_Vector;
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

end LBFGSB1;
