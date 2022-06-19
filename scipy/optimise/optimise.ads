--  Based on scipy/optimize/optimize.py

with Constraints;
with Differentiable_Functions; use Differentiable_Functions;
with NL_Arrays_And_Matrices; use NL_Arrays_And_Matrices;

package Optimise is

   type Direction_Kind is (Random_Direction, All_Direction);
   type Opt_Fun_Access is access function (X : Real_Float_List) return Float;
   type Grad_Func_Access is access function (X : Real_Float_Vector) return Float;

    type Optimise_Result
      (Jac_Length, N_Coor, N : Natural) is record
        Fun      : RF_Fun_Access;
        X        : Real_Float_List;
        Jac      : NL_Arrays_And_Matrices.Real_Float_Vector (1 .. Jac_Length);
        N_It     : Positive := 1;
        N_Fev    : Natural := 0;
        N_Jev    : Natural := 0;
        N_Hev    : Natural := 0;
        SK       : NL_Arrays_And_Matrices.Real_Float_Matrix (1 .. N_Coor,
                                                             1 .. N);
        YK       : NL_Arrays_And_Matrices.Real_Float_Matrix (1 .. N_Coor,
                                                             1 .. N);
        Status   : Natural := 0;
        Success  : Boolean := False;
    end record;

    function Check_Grad
      (Fun    : RF_Fun_Access; Grad_Func : Grad_Func_Access;
      X0 : Real_Float_Vector; Epsilon : Float := 10.0 ** (-8);
      Direction : Direction_Kind := All_Direction) return Float;
    function Prepare_Scalar_Function
      (Fun    : RF_Fun_Access; X0 : Real_Float_Vector;
       Bounds : Constraints.Array_Bounds := Constraints.Default_Bounds;
       Epsilon, Finite_Diff_Rel_Step : Float := 0.0)
      return Scalar_Function;
    function Prepare_Jac_Scalar_Function
      (Fun                           : RF_Fun_Access; X0 : Real_Float_Vector;
       Jac                           : Real_Float_Vector;
       Bounds                        : Constraints.Array_Bounds :=
         Constraints.Default_Bounds;
       Epsilon, Finite_Diff_Rel_Step : Float := 0.0)
      return Scalar_Function;
    function F_Min_BFGS (F : RF_Fun_Access; X0 : Real_Float_Vector)
                        return Optimise_Result;

end Optimise;
