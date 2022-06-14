--  Based on scipy/optimize/lbfgsb_py.py

with Interfaces.Fortran;

with Ada.Assertions; use Ada.Assertions;
with Ada.Containers;

with Lbfgsb_F_Interface;

package body LBFGSB is
--      type Byte is range -128 .. 127;
    type Byte is mod 256;
    for  Byte'Size use 16;

    type S60 is new Interfaces.Fortran.Fortran_Character (1 .. 60);

    function Minimise_LBFGSB (Fun    : Optimise.Opt_Fun_Access;
                              X0     : Stochastic_Optimizers.Parameters_List;
                              Meth   : Opt_Minimise.Method_Type;
                              Bounds : Constraints.Bounds_List :=
                                Constraints.Array_Bounds_Package.Empty_Vector;
                              Max_Cor : Positive := 10;
                              Ftol    : Float :=
                                2.2204460492503131 * 10.0 ** (-09);
                              Gtol     : Float := 10.0 ** (-5);
                              Eps      : Float := 10.0 ** (-8);
                              Max_Fun  : Positive := 15000;
                              Max_Iter : Positive := 15000;
                              Options  : Opt_Minimise.Minimise_Options :=
                                Opt_Minimise.No_Options)
                              return Optimise.Optimise_Result is
        use Interfaces.Fortran;
        use Ada.Containers;
        use Stochastic_Optimizers;
        use Lbfgsb_F_Interface;
        Routine_Name : constant String := "LBFGSB.Minimise_LBFGSB";
        X0_Length    : constant Positive := Positive (X0.Length);
        I_Print        : constant Integer := -1;  --  L273
        X              : Parameters_List := X0;
        Num_Iterations : Natural := 0;
        M              : Integer := Max_Cor;
        Nbd            : array (1 .. X0_Length) of
          Interfaces.Fortran.Fortran_Integer := (others => 0);
        Low_Bound      : array (1 .. X0_Length) of Integer := (others => 0);
        Upper_Bound    : array (1 .. X0_Length) of Integer := (others => 0);
        F              : Interfaces.Fortran.Double_Precision := 0.0;
        G                                  : array (1 .. X0_Length) of Integer :=
                                               (others => 0);
        PGtol          : Float := Gtol;
        Factor                             : Float := Ftol / Eps;
        wa             : array (1 .. 2 * Max_Cor * X0_Length + 5 * X0_Length
                                + 11 * Max_Cor ** 2 + 8 * Max_Cor) of Float :=
                           (others => 0.0);
        iwa            : array (1 .. 3 * X0_Length) of Fortran_Integer :=
                           (others => 0);
        L_Save         : Fortran_LSave_Array := (others => 0);
        I_Save         : Fortran_Integer_Array (1 .. 44) := (others => 0);
        D_Save         : Fortran_DSave_Array := (others => 0.0);
        C_Save         : String := "";
        Task_Name      : String := "START";
        Continiue      : Boolean := True;
        Result       : Optimise.Optimise_Result;
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

        --  L309

        --  L361
        while Continiue loop
            Lbfgsb_F_Interface.Setulb
              (Fortran_Integer (M), Fortran_DP_Array (X),
               Fortran_DP_Array (Low_Bound), Fortran_DP_Array (Upper_Bound),
               Fortran_Integer_Array (nbd), f, Fortran_DP_Array (g),
               Double_Precision (factor), Double_Precision (pgtol),
               Double_Precision (wa), Double_Precision (iwa),
               To_Fortran (Task_Name), Fortran_Integer (I_Print),
               To_Fortran (C_Save), L_Save, I_Save, D_Save,
                Fortran_Integer (Options.Max_Line_Steps));
        end loop;

        return Result;

    end Minimise_LBFGSB;

end LBFGSB;
