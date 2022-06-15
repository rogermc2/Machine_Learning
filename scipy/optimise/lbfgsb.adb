--  Based on scipy/optimize/lbfgsb_py.py

with Interfaces.Fortran;

with Ada.Assertions; use Ada.Assertions;
--  with Ada.Containers;

with Differentiable_Functions;
with Lbfgsb_F_Interface;

package body LBFGSB is
   --      type Byte is range -128 .. 127;
   --     type Byte is mod 256;
   --     for  Byte'Size use 16;

   --     type S60 is new Interfaces.Fortran.Fortran_Character (1 .. 60);

   function Minimise_LBFGSB (Fun      : Optimise.Opt_Fun_Access;
                             X0       : Stochastic_Optimizers.Parameters_List;
                             Meth     : Opt_Minimise.Method_Type;
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
      use Interfaces.Fortran;
      --        use Ada.Containers;
--        use Stochastic_Optimizers;
      use Differentiable_Functions;
      use Lbfgsb_F_Interface;
      Routine_Name   : constant String := "LBFGSB.Minimise_LBFGSB";
      X0_Length      : constant Positive := Positive (X0.Length);
      I_Print        : constant Fortran_Integer := -1;  --  L273
      X              : Fortran_DP_Array (1 .. X0_Length);
      --          X              : Parameters_List := X0;
      Num_Iterations : Natural := 0;
      M              : Fortran_Integer := Fortran_Integer (Max_Cor);
      Nbd            : Fortran_Integer_Array (1 .. X0_Length) :=
                         (others => 0);
      Low_Bound      : Fortran_DP_Array (1 .. X0_Length) := (others => 0.0);
      Upper_Bound    : Fortran_DP_Array (1 .. X0_Length) := (others => 0.0);
      F              : Double_Precision := 0.0;
      G              : Fortran_DP_Array (1 .. X0_Length) := (others => 0.0);
      PGtol          : Double_Precision := Double_Precision (Gtol);
      Factor         : Double_Precision := Double_Precision (Ftol / Eps);
      Wa_Length      : Integer := 2 * Max_Cor * X0_Length + 5 * X0_Length
                         + 11 * Max_Cor ** 2 + 8 * Max_Cor;
      Wa             : Fortran_DP_Array (1 .. wa_Length) :=  (others => 0.0);
      I_Wa            : Fortran_Integer_Array (1 .. 3 * X0_Length) :=
                         (others => 0);
      L_Save         : Fortran_LSave_Array := (others => 0);
      I_Save         : Fortran_Integer_Array (1 .. 44) := (others => 0);
      D_Save         : Fortran_DSave_Array := (others => 0.0);
      C_Save         : Character_60 := To_Fortran ("");
      Task_Name      : Character_60 := To_Fortran ("START");
      Scalar_Func    : Differentiable_Functions.Scalar_Function (X0_Length);
      Continiue      : Boolean := True;
      Result         : Optimise.Optimise_Result;
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
           (M, X, Low_Bound, Upper_Bound, nbd, f, G,
            Factor, Pgtol, Wa, I_Wa, Task_Name, I_Print, C_Save, L_Save, I_Save,
            D_Save, Fortran_Integer (Options.Max_Line_Steps));
         if Task_Name (1 .. 2) = "FG" then
            Scalar_Func := Optimise.Prepare_Scalar_Function;
            Fun_And_Grad (Scalar_Func, F, G);
         elsif Task_Name (1 .. 5) = "NEW_X" then
            Scalar_Func := Optimise.Prepare_Scalar_Function;
         else
            Continiue := False;
         end if;
      end loop;

      return Result;

   end Minimise_LBFGSB;

   --  ------------------------------------------------------------------------

end LBFGSB;
