--  Based on scipy/optimize/lbfgsb_py.py

with Interfaces.Fortran;

with Ada.Assertions; use Ada.Assertions;
with Ada.Containers;

with Lbfgsb_F_Interface;

package body LBFGSB is

    function Minimise_LBFGSB (Fun    : Multilayer_Perceptron.Max_Function_Access;
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
        use Ada.Containers;
        use Stochastic_Optimizers;
        Routine_Name : constant String := "LBFGSB.Minimise_LBFGSB";
        X0_Length    : constant Positive := Positive (X0.Length);
      X            : Parameters_List := X0;
      Num_Iterations : Natural := 0;
      Nbd            : array (1 .. X0_Length) of
        Interfaces.Fortran.Fortran_Integer := (others => 0);
      Low_Bound       : array (1 .. X0_Length) of Integer := (others => 0);
      Upper_Bound     : array (1 .. X0_Length) of Integer := (others => 0);
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
         Lbfgsb_F_Interface.setulb (m, x, Low_Bound, Upper_Bound, nbd, f, g, factr,
                       pgtol, wa, iwa, Task_Name, iprint, csave, lsave,
                       isave, dsave, maxls);
      end loop;

      return Result;

    end Minimise_LBFGSB;

end LBFGSB;
