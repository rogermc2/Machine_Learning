--  Based on scipy/optimize/lbfgsb_py.py

with Ada.Assertions; use Ada.Assertions;

with NL_Arrays_And_Matrices; use NL_Arrays_And_Matrices;
with Differentiable_Functions;

package body LBFGSB1 is
   --      type Byte is range -128 .. 127;
   --     type Byte is mod 256;
   --     for  Byte'Size use 16;

   --     type S60 is new Interfaces.Fortran.Fortran_Character (1 .. 60);

   --      type Lbfgs_Inv_Hess_Product (N_Coor, N : Positive) is record
   --          SK : NL_Arrays_And_Matrices.Real_Float_Matrix (1 .. N_Coor, 1 .. N);
   --          YK : NL_Arrays_And_Matrices.Real_Float_Matrix (1 .. N_Coor, 1 .. N);
   --      end record;
   function Parameters_List_To_DP_Array
     (PL : Stochastic_Optimizers.Parameters_List) return Fortran_DP_Array;

   --  ------------------------------------------------------------------------

   function All_Close (A, B  : Fortran_DP_Array;
                       A_Tol : Double_Precision := 10.0 ** (-8))
                       return Boolean is
      Result : Boolean := True;
   begin
      for index in A'Range loop
         Result := Result and abs(A (index) - B (index)) < A_Tol;
      end loop;

      return Result;

   end All_Close;

   --  ------------------------------------------------------------------------

   function Minimise_LBFGSB (Fun      : Num_Diff.Deriv_Float_Fun_Access;
                             X0       : Stochastic_Optimizers.Parameters_List;
                             Bounds   : Constraints.Bounds_List :=
                               Constraints.Array_Bounds_Package.Empty_Vector;
                             Max_Cor  : Positive := 10;
                             Ftol     : Float :=
                               2.2204460492503131 * 10.0 ** (-09);
                             Gtol     : Float := 10.0 ** (-5);
                             Eps      : Float := 10.0 ** (-8);
                             Max_Fun  : Positive := 15000;
                             Max_Iter : Positive := 15000;
                             Options  : Opt_Minimise.Minimise_Options :=
                               Opt_Minimise.No_Options)
                             return Optimise.Optimise_Result is
      use Differentiable_Functions;
      Routine_Name    : constant String := "LBFGSB.Minimise_LBFGSB";
      X0_Length       : constant Positive := Positive (X0.Length);
      I_Print         : constant Fortran_Integer := -1;  --  L273
      X               : Fortran_DP_Array := Parameters_List_To_DP_Array (X0);
      RF_X            : Real_Float_Vector (1 .. X0_Length) := (others => 0.0);
      Num_Iterations  : Natural := 0;
      M               : constant Fortran_Integer := Fortran_Integer (Max_Cor);
      --  L331 nbd[i] = bounds_map[l, u] => bounds_map[1, 1] = 2
      Nbd             : constant Fortran_Integer_Array (1 .. X0_Length) :=
                          (others => 2);
      Low_Bound       : Fortran_DP_Array (1 .. X0_Length) := (others => 0.0);
      Upper_Bound     : Fortran_DP_Array (1 .. X0_Length) := (others => 0.0);
      F               : Double_Precision := 0.0;
      G               : Fortran_DP_Array (1 .. X0_Length) := (others => 0.0);
      RF_G            : Real_Float_Vector (1 .. X0_Length) := (others => 0.0);
      PGtol           : Double_Precision := Double_Precision (Gtol);
      Factor          : Double_Precision := Double_Precision (Ftol / Eps);
      Wa_Length       : constant Positive := 2 * Max_Cor * X0_Length + 5 * X0_Length
                          + 11 * Max_Cor ** 2 + 8 * Max_Cor;
      Wa              : Fortran_DP_Array (1 .. wa_Length) :=  (others => 0.0);
      I_Wa            : Fortran_Integer_Array (1 .. 3 * X0_Length) :=
                          (others => 0);
      L_Save          : Fortran_LSave_Array := (others => 0);
      I_Save          : Fortran_Integer_Array (1 .. 44) := (others => 0);
      D_Save          : Fortran_DSave_Array := (others => 0.0);
      C_Save          : Character_60 := To_Fortran ("");
      Task_Name       : Character_60 := To_Fortran ("START");
      Scalar_Func     : Differentiable_Functions.Scalar_Function (X0_Length, 1);
      Continiue       : Boolean := True;
      Warn_Flag       : Natural;
      Result          : Optimise.Optimise_Result (0, 0, 0);
   begin
      --  L266
      if not Bounds.Is_Empty then
         Assert (Positive (Bounds.Length) = X0_Length, Routine_Name &
                   "Bounds and X0 have different lengths.");
      end if;

      for row in Bounds.First_Index .. Bounds.Last_Index loop
         null;
         --              for col in X'Range (2) loop
         --                  if X.Element (row). < Bounds.Lower.Element (row) then
         --                      X (row) := Bounds.Lower.Element (row);
         --                  elsif X (row) > Bounds.Upper.Element (row) then
         --                      X (row) := Bounds.Upper.Element (row);
         --                  end if;
         --              end loop;
      end loop;

      --  L323
      for index in 1 .. X0_Length loop
         Low_Bound (index) := Double_Precision (Bounds (index).Lower);
         Upper_Bound (index) := Double_Precision (Bounds (index).Upper);
      end loop;

      --  L349
      Num_Iterations := 0;
      while Continiue loop
         Lbfgsb_F_Interface.Setulb
           (M, X, Low_Bound, Upper_Bound, nbd, f, G,
            Factor, Pgtol, Wa, I_Wa, Task_Name, I_Print, C_Save, L_Save, I_Save,
            D_Save, Fortran_Integer (Options.Max_Line_Steps));
         RF_X := To_RF_Array (X);
         if Task_Name (1 .. 2) = "FG" then
            Scalar_Func := Optimise.Prepare_Scalar_Function (Fun, RF_X);
            RF_G := To_RF_Array (G);
            Fun_And_Grad (Scalar_Func, RF_X, F, RF_G);
         elsif Task_Name (1 .. 5) = "NEW_X" then
            Num_Iterations := Num_Iterations + 1;
            Scalar_Func := Optimise.Prepare_Scalar_Function (Fun, RF_X);
            if Num_Iterations > Max_Iter then
               Task_Name (1 .. 43) :=
                 "STOP: TOTAL NO. of ITERATIONS REACHED LIMIT";
            elsif Scalar_Func.N_Fev > Max_Fun then
               Task_Name (1 .. 52) :=
                 "STOP: TOTAL NO. of f AND g EVALUATIONS Exceeds LIMIT";
            end if;
         else
            Continiue := False;
         end if;
      end loop;

      if Task_Name (1 .. 4) = "CONV" then
         Warn_Flag := 0;
      elsif Scalar_Func.N_Fev > Max_Fun or Num_Iterations > Max_Iter then
         Warn_Flag := 1;
      else
         Warn_Flag := 2;
      end if;

      --  L387
      declare
         MN         : constant Positive := Positive (M) * X0_Length - 1;
         --              Bfgs_Iters : constant Positive := Positive (I_Save (31));
         --              Num_Corrs  : constant Positive :=
         --                             Positive'Min (Bfgs_Iters, Max_Cor);
         S          : Real_Float_Matrix (1 .. Positive (M), 1 .. X0_Length);
         Y          : Real_Float_Matrix (1 .. Positive (M), 1 .. X0_Length);
         --              Hess_Inv   : Lbfgs_Inv_Hess_Product (Positive (M), X0_Length);
         Result_J   : Optimise.Optimise_Result
           (G'Length, Positive (M), X0_Length);
      begin
         --  wa is a double precision working array of length
         --       (2mmax + 5)nmax + 12mmax^2 + 12mmax.
         for row in S'First .. S'Last loop
            for col in  S'First (2) .. S'Last (2) loop
               S (row, col) := Float (Wa (row + (col - 1) * X0_Length));
            end loop;
         end loop;

         for row in Y'First .. Y'Last loop
            for col in  Y'First (2) .. Y'Last (2) loop
               Y (row, col) :=
                 Float (Wa (MN + row + (col - 1) * X0_Length));
            end loop;
         end loop;

         --              Hess_Inv.SK := S;
         --              Hess_Inv.YK := Y;
         Result_J.Fun := Fun;
         for index in G'Range loop
            Result_J.Jac (index) := Float (G (index));
         end loop;
         Result_J.N_Fev := Scalar_Func.N_Fev;
         Result_J.N_Jev := Scalar_Func.N_Gev;
         Result_J.N_It := Num_Iterations;
         Result_J.Status := Warn_Flag;
         Result_J.Success := Warn_Flag = 0;
         Result_J.SK := S;
         Result_J.YK := Y;
         Result_J.X.Clear;
         for index in X'First .. X'Last loop
            Result_J.X.Append (Float (X (index)));
         end loop;

         Result := Result_J;
      end;

      return Result;

   end Minimise_LBFGSB;

   --  ------------------------------------------------------------------------

   function Parameters_List_To_DP_Array
     (PL : Stochastic_Optimizers.Parameters_List) return Fortran_DP_Array is
      DP_Length : Natural := 0;
   begin
      for index in PL.First_Index .. PL.Last_Index loop
         declare
            Params : constant Stochastic_Optimizers.Parameters_Record :=
                       PL (index);
         begin
            DP_Length := DP_Length + Params.Num_Rows * (Params.Num_Cols + 1);
         end;
      end loop;

      declare
         Result   : Fortran_DP_Array (1 .. DP_Length);
         DP_Index : Natural := 0;
      begin
         for index in PL.First_Index .. PL.Last_Index loop
            declare
               Params    : constant Stochastic_Optimizers.Parameters_Record :=
                             PL (index);
               Coeffs_1D : constant Real_Float_Vector :=
                             Flatten (Params.Coeff_Gradients);
            begin
               for coeff in Coeffs_1D'Range loop
                  DP_Index := DP_Index + 1;
                  Result (DP_Index) := Double_Precision (Coeffs_1D (coeff));
               end loop;

               for int in Params.Intercept_Grads'Range loop
                  DP_Index := DP_Index + 1;
                  Result (DP_Index) :=
                    Double_Precision (Params.Intercept_Grads (int));
               end loop;
            end;
         end loop;

         return Result;
      end;

   end Parameters_List_To_DP_Array;

   --  ------------------------------------------------------------------------

end LBFGSB1;
