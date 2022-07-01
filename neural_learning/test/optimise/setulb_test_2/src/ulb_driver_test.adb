--  Based on https://github.com/funsim/lbfgsb/blob/master/driver1.f
--  This simple driver demonstrates how to call the L-BFGS-B code to
--  solve a sample problem (the extended Rosenbrock function
--  subject to bounds on the variables).
--  The dimension n of this problem is variable.

--  with Ada.Assertions;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;

with Lbfgsb_F_Interface; use Lbfgsb_F_Interface;
with NL_Arrays_And_Matrices; use  NL_Arrays_And_Matrices;

procedure ULB_Driver_Test is
   Routine_Name : constant String := "ULB_Driver_Test ";
   --  dimension of the largest problem to be solved
   N_Max        : constant Integer := 1024;
   --  maximum number of limited memory corrections
   M_Max        : constant Integer := 17;
   --  the dimension n of the sample problem and the number m of limited memory
   --  corrections stored
   N            : constant Integer := 25;
   M            : constant Integer := 5;
   Max_Ls       : constant Integer := 20;
   Factor       : Float := 10.0 ** 7;
   Pg_Tol       : Float := 10.0 ** (-5);
   Nbd                : Integer_Array (1 .. N_Max) := (others => 0);
   Lower_Bound        : Real_Float_Vector (1 .. N_Max) := (others => 0.0);
   Upper_Bound        : Real_Float_Vector (1 .. N_Max) := (others => 1.0);
   X                  : Real_Float_Vector (1 .. N_Max);
   F                  : Float := 0.0;
   G                  : Real_Float_Vector (1 .. N) := (others => 0.0);
   WA_Length          : constant Positive := 2 * M_Max * N_Max +
                          5 * N_Max + 11 * M_Max ** 2 + 8 * M_Max;
   WA                 : Real_Float_Vector (1 .. WA_Length) := (others => 0.0);
   IWA                : Integer_Array (1 .. 3 * N_Max);
   Task_Name          : Unbounded_String;
   C_Save             : S60 := (others => '0');
   L_Save             : LSave_Array := (others => 0);
   I_Save             : Integer_Array (1 .. 44) := (others => 0);
   D_Save             : DSave_Array := (others => 0.0);
   T1                 : Float;
   T2                 : Float;
   Continue           : Boolean := True;
begin
   Put (Routine_Name);
   for index in 1 .. N loop
      Nbd (index) := 2;
      Upper_Bound  (index) := 100.0;
      if index mod 2 = 0 then
         Lower_Bound  (index) := 1.0;
      else
         Lower_Bound  (index) := 100.0;
      end if;
      X (index) := 3.0;
   end loop;

   Task_Name := To_Unbounded_String ("START");
   while Continue loop
      Set_Ulb (N, M, X, Lower_Bound, Upper_Bound, Nbd, F, G, Factor, Pg_Tol,
              WA, IWA, C_Save, Task_Name, L_Save, I_Save, D_Save, Max_Ls);
      Put_Line ("Task_Name: " & To_String (Task_Name));
      if Slice (Task_Name, 1, 2) = "FG" then
         --  the minimization routine has returned to request the
         --  function f and gradient g values at the current x.
         F := 0.25 * (X (1) - 1.0) ** 2;
         for index in 2 .. N loop
            F := F + (X (index) - X (index - 1 ) ** 2) ** 2;
         end loop;
         F := 4.0 * F;

         T1 := X (2) - X (1) ** 2;
         G (1) := 2.0 * (X (1) - 1.0) ** 2 - 16.0 * X (1) * T1;
         for index in 2 .. N - 1 loop
            T2 := T1;
            T1 := X (index + 1) - X (index) ** 2;
            G (index) := 8.0 * T2 - 16.0 * X (index) * T1;
         end loop;
         G (N) := 8.0 * T1;
      elsif Slice (Task_Name, 1, 5) = "New_X" then
        null;
      else
         Continue := False;
      end if;
   end loop;

end ULB_Driver_Test;
