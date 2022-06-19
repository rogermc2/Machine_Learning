--  Based on /scipy/scipy/optimize/tests/test_lbfgsb_setulb.py

with Interfaces.Fortran; use Interfaces.Fortran;

with Ada.Assertions; use Ada.Assertions;
with Ada.Text_IO; use Ada.Text_IO;

with Lbfgsb1; use Lbfgsb1;
with Lbfgsb_F_Interface; use Lbfgsb_F_Interface;

--  L69 test_setulb_floatround
procedure Test_Set_ULB is
   Routine_Name : constant String := "Test_Set_UlB ";
   N            : constant Integer := 5;
   M            : constant Integer := 10;

   procedure Obj_Fun (X : Fortran_DP_Array; F : out Double_Precision;
                      G : out Fortran_DP_Array) is
      X0    : constant Fortran_DP_Array (1 .. 5) := (0.8750000000000278,
                                                     0.7500000000000153,
                                                     0.9499999999999722,
                                                     0.8214285714285992,
                                                     0.6363636363636085);
      X1    : constant Fortran_DP_Array (1 .. 5) := (1.0, 0.0, 1.0, 0.0, 0.0);
      X2    : constant Fortran_DP_Array (1 .. 5) := (1.0,
                                                     0.0,
                                                     0.9889733043149325,
                                                     0.0,
                                                     0.026353554421041155);
      X3    : constant Fortran_DP_Array (1 .. 5) := (1.0,
                                                     0.0,
                                                     0.9889917442915558,
                                                     0.0,
                                                     0.020341986743231205);
      F0    : constant Double_Precision := 5163.647901211178;
      F1    : constant Double_Precision := 5149.8181642072905;
      F2    : constant Double_Precision := 5149.379332309634;
      F3    : constant Double_Precision := 5149.374490771297;
      G0    : constant Fortran_DP_Array (1 .. 5) := (-0.5934820547965749,
                                                     1.6251549718258351,
                                                     -71.99168459202559,
                                                     5.346636965797545,
                                                     37.10732723092604);
      G1    : constant Fortran_DP_Array (1 .. 5) := (-0.43295349282641515,
                                                     1.008607936794592,
                                                     18.223666726602975,
                                                     31.927010036981997,
                                                     -19.667512518739386);
      G2    : constant Fortran_DP_Array (1 .. 5) := (-0.4699874455100256,
                                                     0.9466285353668347,
                                                     -0.016874360242016825,
                                                     48.44999161133457,
                                                     5.819631620590712);
      G3    : constant Fortran_DP_Array (1 .. 5) := (-0.46970678696829116,
                                                     0.9612719312174818,
                                                     0.006129809488833699,
                                                     48.43557729419473,
                                                     6.005481418498221);
   begin
      if All_Close (X, X0) then
         F := F0;
         G := G0;
      elsif All_Close (X, X1) then
         F := F1;
         G := G1;
      elsif All_Close (X, X2) then
         F := F2;
         G := G2;
      elsif All_Close (X, X3) then
         F := F3;
         G := G3;

      else
         Assert (False,
                 "The simplified objective function is not defined at the " &
                   "requested point");
      end if;
   end Obj_Fun;

   Max_Ls             : constant Fortran_Integer := 20;
   Factor             : Double_Precision := 10.0 ** 7;
   Pg_Tol             : Double_Precision := 10.0 ** (-5);
   I_Print            : constant Fortran_Integer := -1;
   Nbd                : constant Fortran_Integer_Array (1 .. N) :=
                          (others => 2);
   Low_Bound          : constant Fortran_DP_Array (1 .. N) := (others => 0.0);
   Upper_Bound        : constant Fortran_DP_Array (1 .. N) := (others => 1.0);
   X0                 : constant Fortran_DP_Array (1 .. 5) :=
                          (0.8750000000000278,
                           0.7500000000000153,
                           0.9499999999999722,
                           0.8214285714285992,
                           0.6363636363636085);
   X                  : Fortran_DP_Array := X0;
   F                  : Double_Precision;
   G                  : Fortran_DP_Array (1 .. N) := (others => 0.0);
   WA_Length          : constant Positive := 2 * M * N + 5 * N + 11 * M** 2 + 8 * M;
   WA                 : Fortran_DP_Array (1 .. WA_Length) := (others => 0.0);
   IWA                : Fortran_Integer_Array (1 .. 3 * N);
   Task_Name          : Character_60;
   C_Save             : Character_60;
   L_Save             : Fortran_LSave_Array := (others => 0);
   I_Save             : Fortran_Integer_Array (1 .. 4) := (others => 0);
   D_Save             : Fortran_DSave_Array := (others => 0.0);
   Pass               : Boolean := True;
begin
   Put (Routine_Name);
   Task_Name (1 .. 5) := To_Fortran ("START");
   for iter in 1 .. 7 loop
      Obj_Fun (X, F, G);
      Setulb (Fortran_Integer (M), X, Low_Bound, Upper_Bound, Nbd, F, G, Factor,
              Pg_Tol, WA, IWA, Task_Name, I_Print, C_Save, L_Save,
              I_Save, D_Save, Max_Ls);
      for index in X'Range loop
         if X (index) > Upper_Bound (index) or
           X (index) < Low_Bound (index) then
            Pass := False;
            Put_Line ("X (" & Integer'Image (index) & ") outside bounds");
         end if;
      end loop;
   end loop;

   if Pass then
      Put_Line ("passed");
   else
      Put_Line ("failed");
   end if;

end Test_Set_UlB;
