--  Based on scipy/optimize/optimize.py

with Constraints;
with Differentiable_Functions; use Differentiable_Functions;
with Lbfgsb_F_Interface;
with NL_Arrays_And_Matrices; use NL_Arrays_And_Matrices;

package Optimise is

    type Opt_Fun_Access is access function (X : Real_Float_List) return Float;

    type Optimise_Result
      (Jac_Length, N_Coor, N : Natural) is record
        Fun      : Opt_Fun_Access;
        X        : Real_Float_List;
        Jac      : Lbfgsb_F_Interface.Fortran_DP_Array (1 .. Jac_Length);
        N_It     : Positive := 1;
        N_Fev    : Natural := 0;
        N_Jev    : Natural := 0;
        N_Hev    : Natural := 0;
        SK       : NL_Arrays_And_Matrices.Real_Float_Matrix (1 .. N_Coor, 1 .. N);
        YK       : NL_Arrays_And_Matrices.Real_Float_Matrix (1 .. N_Coor, 1 .. N);
        Status   : Natural := 0;
        Success  : Boolean := False;
    end record;

    function Prepare_Scalar_Function
      (Fun : DP_Fun_Access; X0 : Lbfgsb_F_Interface.Fortran_DP_Array;
       Bounds : Constraints.Array_Bounds := Constraints.Default_Bounds;
       Epsilon, Finite_Diff_Rel_Step : Float := 0.0)
    return Scalar_Function;
    function Prepare_Jac_Scalar_Function
      (Fun : DP_Fun_Access; X0 : Lbfgsb_F_Interface.Fortran_DP_Array;
       Jac : Lbfgsb_F_Interface.Fortran_DP_Array;
       Bounds : Constraints.Array_Bounds := Constraints.Default_Bounds;
       Epsilon, Finite_Diff_Rel_Step : Float := 0.0)
    return Scalar_Function;
    function F_Min_BFGS (F : DP_Fun_Access; X0 : Lbfgsb_F_Interface.Fortran_DP_Array)
                         return Optimise_Result;

end Optimise;
