--  Based on /scipy/scipy/optimize/tests/test_lbfgsb_setulb.py

with Ada.Assertions; use Ada.Assertions;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;

with L_BFGS_B; use L_BFGS_B;
with Lbfgsb_F_Interface; use Lbfgsb_F_Interface;
with NL_Arrays_And_Matrices; use  NL_Arrays_And_Matrices;

--  L69 test_setulb_floatround
procedure Test_Set_ULB is
   Routine_Name : constant String := "Test_Set_UlB ";
   N            : constant Integer := 5;
   M            : constant Integer := 10;

   procedure Obj_Fun (X : Real_Float_Vector; F : out Float;
                      G : out Real_Float_Vector) is
      X0    : constant Real_Float_Vector (1 .. 5) := (0.8750000000000278,
                                                     0.7500000000000153,
                                                     0.9499999999999722,
                                                     0.8214285714285992,
                                                     0.6363636363636085);
      X1    : constant Real_Float_Vector (1 .. 5) := (1.0, 0.0, 1.0, 0.0, 0.0);
      X2    : constant Real_Float_Vector (1 .. 5) := (1.0,
                                                     0.0,
                                                     0.9889733043149325,
                                                     0.0,
                                                     0.026353554421041155);
      X3    : constant Real_Float_Vector (1 .. 5) := (1.0,
                                                     0.0,
                                                     0.9889917442915558,
                                                     0.0,
                                                     0.020341986743231205);
      F0    : constant Float := 5163.647901211178;
      F1    : constant Float := 5149.8181642072905;
      F2    : constant Float := 5149.379332309634;
      F3    : constant Float := 5149.374490771297;
      G0    : constant Real_Float_Vector (1 .. 5) := (-0.5934820547965749,
                                                     1.6251549718258351,
                                                     -71.99168459202559,
                                                     5.346636965797545,
                                                     37.10732723092604);
      G1    : constant Real_Float_Vector (1 .. 5) := (-0.43295349282641515,
                                                     1.008607936794592,
                                                     18.223666726602975,
                                                     31.927010036981997,
                                                     -19.667512518739386);
      G2    : constant Real_Float_Vector (1 .. 5) := (-0.4699874455100256,
                                                     0.9466285353668347,
                                                     -0.016874360242016825,
                                                     48.44999161133457,
                                                     5.819631620590712);
      G3    : constant Real_Float_Vector (1 .. 5) := (-0.46970678696829116,
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

   Max_Ls             : constant Integer := 20;
   Factor             : Float := 10.0 ** 7;
   Pg_Tol             : Float := 10.0 ** (-5);
   Nbd                : constant Integer_Array (1 .. N) :=
                          (others => 2);
   Low_Bound          : constant Real_Float_Vector (1 .. N) := (others => 0.0);
   Upper_Bound        : constant Real_Float_Vector (1 .. N) := (others => 1.0);
   X0                 : constant Real_Float_Vector (1 .. 5) :=
                          (0.8750000000000278,
                           0.7500000000000153,
                           0.9499999999999722,
                           0.8214285714285992,
                           0.6363636363636085);
   X                  : Real_Float_Vector := X0;
   F                  : Float;
   G                  : Real_Float_Vector (1 .. N) := (others => 0.0);
   WA_Length          : constant Positive := 2 * M * N + 5 * N + 11 * M** 2 + 8 * M;
   WA                 : Real_Float_Vector (1 .. WA_Length) := (others => 0.0);
   IWA                : Integer_Array (1 .. 3 * N);
   Task_Name          : Unbounded_String;
   L_Save             : LSave_Array := (others => 0);
   I_Save             : Integer_Array (1 .. 44) := (others => 0);
   Pass               : Boolean := True;
begin
   Put_Line (Routine_Name);
   Task_Name := To_Unbounded_String ("START");
   for iter in 1 .. 7 loop
      Obj_Fun (X, F, G);
      Set_Ulb (N, M, X, Low_Bound, Upper_Bound, Nbd, F, G, Factor, Pg_Tol,
               WA, IWA, Task_Name, L_Save, I_Save, Max_Ls);

      for index in X'Range loop
         if X (index) > Upper_Bound (index) or
           X (index) < Low_Bound (index) then
            Pass := False;
            Put_Line ("X (" & Integer'Image (index) & ") is outside bounds");
         end if;
      end loop;
   end loop;

   Put (Routine_Name);
   if Pass then
      Put_Line ("passed");
   else
      Put_Line ("failed");
   end if;

end Test_Set_UlB;
