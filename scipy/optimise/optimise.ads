--  Based on scipy/optimize/optimize.py

with Differentiable_Functions;
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
      return Differentiable_Functions.Scalar_Function;
    function F_Min_BFGS (X : Real_Float_Arrays.Real_Vector)
                         return Optimise_Result;

end Optimise;
