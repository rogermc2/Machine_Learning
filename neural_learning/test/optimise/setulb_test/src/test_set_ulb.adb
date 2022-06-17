
--  Based on /scipy/scipy/optimize/tests/test_lbfgsb_setulb.py
--  with Ada.Assertions; use Ada.Assertions;

with Ada.Text_IO; use Ada.Text_IO;

--  with Maths;

--  with Base_Neural;
--  with LBFGSB1; use LBFGSB1;
with Lbfgsb_F_Interface; use Lbfgsb_F_Interface;
--  with Multilayer_Perceptron; use Multilayer_Perceptron;
with NL_Arrays_And_Matrices; use NL_Arrays_And_Matrices;
--  with NL_Types;
--  with Printing;
with Stochastic_Optimizers; use Stochastic_Optimizers;

--  L69 test_setulb_floatround
procedure Test_Set_ULB is
    Routine_Name : constant String := "Test_Set_UlB ";
    Eps          : constant Float := 10.0 ** (-5);
    N            : constant Positive := 5;
    M            : constant Positive := 10;

    procedure Obj_Fun (X : Real_Float_Vector; F, G : out Real_Float_Vector) is

    begin
        F := (others => 0.0);
        G := (others => 0.0);
    end Obj_Fun;

    use Real_Float_Arrays;
    Max_Ls       : constant Positive := 20;
    Factor       : constant Float := 10.0 ** 7;
    Pg_Tol       : constant Float := 10.0 ** (-5);
    I_Print      : constant Integer := -1;
    Nbd          : constant Integer_Array (1 .. N) := (others => 2);
    Low_Bound    : constant Integer_Array (1 .. N) := (others => 0);
    Upper_Bound  : constant Integer_Array (1 .. N) := (others => 1);
    X0           :  Real_Float_Vector (1 .. 5) := (0.8750000000000278,
                                                   0.7500000000000153,
                                                   0.9499999999999722,
                                                   0.8214285714285992,
                                                   0.6363636363636085);
    X            : Real_Float_Vector := X0;
    F            : Real_Float_Vector (1 .. 1);
    G            : Real_Float_Vector (1 .. N) := (others => 0.0);
    WA_Length    : constant Positive := 2 * M * N + 5 * N + 11 * M** 2 + 8 * M;
    WA           : Real_Float_Vector (1 .. WA_Length) := (others => 0.0);
    IWA          : Fortran_Integer_Array (1 .. 3 * N);
    Task_Name    : Character_60 := To_Fortran ("START");
    C_Save       : Character_60 := To_Fortran ("");
    L_Save       : Fortran_Integer_Array (1 .. 4) := (others => 0);
    I_Save       : Fortran_Integer_Array (1 .. 4) := (others => 0);
    D_Save       : Fortran_DP_Array (1 .. 29) := (others => 0.0);
    Params             : Parameters_List;
    Theta              : Parameters_List;
    Loss               : Float;
    pragma Unreferenced (Loss);

    --     function Loss_Grad_Function (Self      : in out MLP_Classifier;
    --                                  Params    : Parameters_List;
    --                                  Y         : Boolean_Matrix;
    --                                  Gradients : out Parameters_List)
    --                                  return Float is
    --     begin
    --        return Loss_Grad_LBFGS (Self, Params, X, Y, Activations, Gradients);
    --
    --     end Loss_Grad_Function;

begin
    Put_Line (Routine_Name);
    f(1) := 0.0;
    for iter in 1 .. 7 loop
        Obj_Fun (X, F, G);
    end loop;

end Test_Set_UlB;
