--  Based on scipy/optimize/lbfgsb_py.py

with Interfaces.Fortran;

with Ada.Assertions; use Ada.Assertions;
with Ada.Containers;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with Lbfgsb_F_Interface;

package body LBFGSB is
--      type Byte is range -128 .. 127;
    type Byte is mod 256;
    for  Byte'Size use 16;

    type S60 is new Interfaces.Fortran.Fortran_Character (1 .. 60);

    function To_Fortran (Text : in out Unbounded_String)
                         return Lbfgsb_F_Interface.Character_60;

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
        I_Print        : constant Fortran_Integer := -1;  --  L273
        X              : Fortran_DP_Array (1 .. X0_Length);
--          X              : Parameters_List := X0;
        Num_Iterations : Natural := 0;
        M              : Fortran_Integer := Fortran_Integer (Max_Cor);
        Nbd            : Fortran_Integer_Array (1 .. X0_Length) :=
                           (others => 0);
        Low_Bound      : Fortran_DP_Array (1 .. X0_Length) := (others => 0.0);
        Upper_Bound    : Fortran_DP_Array (1 .. X0_Length) := (others => 0.0);
        F              : Interfaces.Fortran.Double_Precision := 0.0;
        G              : Fortran_DP_Array (1 .. X0_Length) := (others => 0.0);
        PGtol          : Float := Gtol;
        Factor         : Float := Ftol / Eps;
        wa_Length      : Integer := 2 * Max_Cor * X0_Length + 5 * X0_Length
                                + 11 * Max_Cor ** 2 + 8 * Max_Cor;
        wa             : Fortran_DP_Array (1 .. wa_Length) :=  (others => 0.0);
        iwa            : Fortran_Integer_Array (1 .. 3 * X0_Length) :=
                           (others => 0);
        L_Save         : Fortran_LSave_Array := (others => 0);
        I_Save         : Fortran_Integer_Array (1 .. 44) := (others => 0);
        D_Save         : Fortran_DSave_Array := (others => 0.0);
        C_Save         : Unbounded_String := To_Unbounded_String ("");
        Task_Name      : Unbounded_String := To_Unbounded_String ("START");
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
              (M, X,
               Low_Bound, Upper_Bound,
               Fortran_Integer_Array (nbd), f, Fortran_DP_Array (g),
               Double_Precision (factor), Double_Precision (pgtol),
               wa, iwa, To_Fortran (Task_Name), I_Print, To_Fortran (C_Save),
               L_Save, I_Save, D_Save,
                Fortran_Integer (Options.Max_Line_Steps));
        end loop;

        return Result;

    end Minimise_LBFGSB;

    --  ------------------------------------------------------------------------

    function To_Fortran (Text : in out Unbounded_String)
                         return Lbfgsb_F_Interface.Character_60 is
       F_String : Lbfgsb_F_Interface.Character_60;
    begin
        return F_String;

    end To_Fortran;

end LBFGSB;
