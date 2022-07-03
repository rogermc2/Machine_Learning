--  Based on scipy/optimize/lbfgsb_py.py

with Ada.Assertions; use Ada.Assertions;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;

with Differentiable_Functions;
with Lbfgsb_F_Interface; use Lbfgsb_F_Interface;

package body L_BFGS_B is
--      type Byte is range -128 .. 127;
--     type Byte is mod 256;
--     for  Byte'Size use 16;

    --     type S60 is new Interfaces.Fortran.Fortran_Character (1 .. 60);

    function Parameters_List_To_RF_Array
      (PL : Stochastic_Optimizers.Parameters_List) return Real_Float_Vector;
    --     function To_Real_Float_Vector (DP_Vec : Fortran_DP_Array)
    --                                    return Real_Float_Vector;

    --  ------------------------------------------------------------------------

    function All_Close (A, B  : Real_Float_Vector;
                        A_Tol : Float := 10.0 ** (-8))
                       return Boolean is
        Result : Boolean := True;
    begin
        for index in A'Range loop
            Result := Result and abs (A (index) - B (index)) < A_Tol;
        end loop;

        return Result;

    end All_Close;

    --  ------------------------------------------------------------------------

    function Minimise_LBFGSB (Fun      : Multilayer_Perceptron.Loss_Grad_Access;
                               Args     : Multilayer_Perceptron.Loss_Grad_Args;
                               X0       : Stochastic_Optimizers.Parameters_List;
                               Bounds   : Opt_Constraints.Bounds_List :=
                                 Opt_Constraints.Array_Bounds_Package.Empty_Vector;
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
        Routine_Name    : constant String := "L_BFGS_B.Minimise_LBFGSB ";
        N               : constant Positive := Positive (X0.Length);
        X               : Real_Float_Vector := Parameters_List_To_RF_Array (X0);
        X_Length        : constant Positive := Positive (X'Length);
        Num_Iterations  : Natural := 0;
        M               : constant Integer := Max_Cor;
        --  L331 nbd[i] = bounds_map[l, u] => bounds_map[1, 1] = 2
        Nbd             : constant Integer_Array (1 .. X_Length) :=
                            (others => 2);
        Low_Bound       : Real_Float_Vector (1 .. X_Length) :=
                            (others => Float'Safe_First);
        Upper_Bound     : Real_Float_Vector (1 .. X_Length) :=
                            (others => Float'Safe_Last);
        F_Float         : Float := 0.0;
        G               : Real_Float_Vector (1 .. X_Length) := (others => 0.0);
        PGtol           : Float := Gtol;
        Factor          : Float := Ftol / Eps;
        Wa_Length       : constant Positive := 2 * Max_Cor * X_Length +
                            5 * X_Length + 11 * Max_Cor ** 2 + 8 * Max_Cor;
        Wa              : Real_Float_Vector (1 .. wa_Length) :=  (others => 0.0);
        I_Wa            : Integer_Array (1 .. 3 * X_Length) := (others => 0);
        L_Save          : LSave_Array := (others => 0);
        I_Save          : Integer_Array (1 .. 44) := (others => 0);
        Task_Name       : Unbounded_String := To_Unbounded_String ("START");
        --  L306
        Scalar_Func     : Differentiable_Functions.Scalar_Function :=
                            Optimise.Prepare_Scalar_Function (Fun, X);
        Continue        : Boolean := True;
        Warn_Flag       : Natural;
    begin
        New_Line;
        --  Put_Line (Routine_Name & "L266 N" & Integer'Image (X0_Length));
        --  L266
        if not Bounds.Is_Empty then
            Assert (Positive (Bounds.Length) = N, Routine_Name &
                      "Bounds and X0 have different lengths.");
            for l in Low_Bound'Range loop
                Low_Bound (l) := Bounds.Element (l).Lower;
                Upper_Bound (l) := Bounds.Element (l).Upper;
            end loop;
        end if;

        for row in X'Range loop
            if X (row) < Low_Bound (row) then
                X (row) := Low_Bound (row);
            elsif X (row) > Upper_Bound (row) then
                X (row) := Upper_Bound (row);
            end if;
        end loop;

        --  L306
        Scalar_Func := Optimise.Prepare_Scalar_Function (Fun, X);

        --  L349
        Num_Iterations := 0;
        while Continue loop
            Set_Ulb (X'Length, M, X, Low_Bound, Upper_Bound, Nbd, F_Float, G,
                     Factor, Pgtol, Wa, I_Wa, Task_Name, L_Save, I_Save,
                     Options.Max_Line_Steps);
            Put_Line (Routine_Name & "L356 Task_Name: " & To_String (Task_Name));
            if Slice (Task_Name, 1, 2) =  "FG" then
                --  Overwrite F and G:
                Fun_And_Grad (Scalar_Func, Args, F_Float, G);
                --  L369
            elsif Slice (Task_Name, 1, 5) =  "NEW_X" then
                --  the minimization routine has returned with a new iterate.
                Num_Iterations := Num_Iterations + 1;
                if Num_Iterations > Max_Iter then
                    Task_Name := To_Unbounded_String
                      ("STOP: TOTAL NO. of ITERATIONS REACHED LIMIT");
                elsif Scalar_Func.N_Fev > Max_Fun then
                    Task_Name := To_Unbounded_String
                      ("STOP: TOTAL NO. of f AND g EVALUATIONS Exceeds LIMIT");
                end if;
            else
                Assert (Slice (Task_Name, 1, 6) /=  "ERROR:", Routine_Name &
                          To_String (Trim (Task_Name, Ada.Strings.Both)));
                Continue := False;
            end if;
        end loop;  --  Task_Name starts with FG or New_X

        Put_Line (Routine_Name & "L378 Task_Name: " & To_String (Task_Name));
--          for index in 1 .. 16 loop
--              Put_Line ("I_Save" & Integer'Image (Index) & ":" &
--                          Integer'Image (I_Save (Index)));
--          end loop;

        --  L376
        if Task_Name = To_Unbounded_String ("CONV") then
            Warn_Flag := 0;
        elsif Scalar_Func.N_Fev > Max_Fun or Num_Iterations > Max_Iter then
            Warn_Flag := 1;
        else
            Warn_Flag := 2;
        end if;

--          Put_Line (Routine_Name & "L385 current iteration number:" &
--                      Integer'Image (I_Save (30)));
        Put_Line (Routine_Name & "L385 Max_Cor" & Integer'Image (Max_Cor));
        --  isave(31) = the total number of BFGS updates prior to the current
        --  iteration;
        declare
            MN           : constant Positive := Positive (M) * N - 1;
            Bfgs_Updates : constant Positive := Positive (I_Save (31) + 1);
            Num_Corrs    : constant Natural :=
                           Integer'Min (Bfgs_Updates, Max_Cor);
            S            : Real_Float_Matrix (1 .. Num_Corrs, 1 .. N);
            Y            : Real_Float_Matrix (1 .. Num_Corrs, 1 .. N);
            Hess_Inv     : Optimise.Lbfgs_Inv_Hess_Product (Num_Corrs, N);
            Result       : Optimise.Optimise_Result (X'Length, Num_Corrs, N);
        begin
            --  L385
            --  wa is a double precision working array of length
            --       (2mmax + 5)nmax + 12mmax^2 + 12mmax.
            for row in S'First .. S'Last loop
                for col in  S'First (2) .. S'Last (2) loop
                    S (row, col) := Wa (row + (col - 1) * N);
                end loop;
            end loop;

            for row in Y'First .. Y'Last loop
                for col in  Y'First (2) .. Y'Last (2) loop
                    Y (row, col) := Wa (MN + row + (col - 1) * N);
                end loop;
            end loop;

            --  L395
            Hess_Inv.SK := S;
            Hess_Inv.YK := Y;
            Result.Hess_Inv := Hess_Inv;

            Result.Fun := F_Float;
            for index in G'Range loop
                Result.Jac (index) := G (index);
            end loop;

            Result.N_Fev := Scalar_Func.N_Fev;
            Result.N_Jev := Scalar_Func.N_Gev;
            Result.N_It := Num_Iterations;
            Result.Status := Warn_Flag;
            Result.Success := Warn_Flag = 0;
            Result.X := X;
            return Result;
        end;

    end Minimise_LBFGSB;

    --  ------------------------------------------------------------------------

    function Parameters_List_To_RF_Array
      (PL : Stochastic_Optimizers.Parameters_List) return Real_Float_Vector is
        RF_Length : Natural := 0;
    begin
        for index in PL.First_Index .. PL.Last_Index loop
            declare
                Params : constant Stochastic_Optimizers.Parameters_Record :=
                           PL (index);
            begin
                RF_Length := RF_Length + Params.Num_Rows * (Params.Num_Cols + 1);
            end;
        end loop;

        declare
            Result   : Real_Float_Vector (1 .. RF_Length);
            RF_Index : Natural := 0;
        begin
            for index in PL.First_Index .. PL.Last_Index loop
                declare
                    Params    : constant Stochastic_Optimizers.Parameters_Record :=
                                  PL (index);
                    Coeffs_1D : constant Real_Float_Vector :=
                                  Flatten (Params.Coeff_Gradients);
                begin
                    for coeff in Coeffs_1D'Range loop
                        RF_Index := RF_Index + 1;
                        Result (RF_Index) := Coeffs_1D (coeff);
                    end loop;

                    for int in Params.Intercept_Grads'Range loop
                        RF_Index := RF_Index + 1;
                        Result (RF_Index) := Params.Intercept_Grads (int);
                    end loop;
                end;
            end loop;

            return Result;
        end;

    end Parameters_List_To_RF_Array;

    --  ------------------------------------------------------------------------

end L_BFGS_B;
