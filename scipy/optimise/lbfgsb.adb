--  Based on scipy/optimize/lbfgsb_py.py

with Interfaces.Fortran;

with Ada.Assertions; use Ada.Assertions;
--  with Ada.Containers;

with Lbfgsb_F_Interface;

package body LBFGSB is
   --      type Byte is range -128 .. 127;
--     type Byte is mod 256;
--     for  Byte'Size use 16;

   function Minimise_LBFGSB (Fun      : Multilayer_Perceptron.Max_Function_Access;
                             X0       : Real_Float_Matrix;
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
      use Stochastic_Optimizers;
      use Lbfgsb_F_Interface;
      Routine_Name          : constant String := "LBFGSB.Minimise_LBFGSB";
      I_Print               : constant Fortran_Integer := -1;  --  L273
      X                     : Fortran_DP_Array := To_DP_Array (Flatten (X0));
      X_Length              : constant Positive := X'Length;
      Num_Iterations        : Natural := 0;
      M                     : Fortran_Integer := Fortran_Integer (Max_Cor);
      Nbd                   : Fortran_Integer_Array (1 .. X_Length) := (others => 0);
      Low_Bound             : Fortran_DP_Array (1 .. X_Length) := (others => 0.0);
      Upper_Bound           : Fortran_DP_Array (1 .. X_Length) := (others => 0.0);
      F                     : Double_Precision := 0.0;
      G                     : Fortran_DP_Array (1 .. X_Length) := (others => 0.0);
      PGtol                 : Double_Precision :=Double_Precision (Gtol);
      Factor                : Double_Precision := Double_Precision (Ftol / Eps);
      wa                    : Fortran_DP_Array
        (1 .. 2 * Max_Cor * X_Length + 5 * X_Length + 11 * Max_Cor ** 2 +
         8 * Max_Cor) := (others => 0.0);
      iwa                   : Fortran_Integer_Array (1 .. 3 * X_Length) :=
                                (others => 0);
      L_Save                : Fortran_LSave_Array := (others => 0);
      I_Save                : Fortran_Integer_Array (1 .. 44) := (others => 0);
      D_Save                : Fortran_DSave_Array := (others => 0.0);
      C_Save                : Character_60;
      Task_Name             : Character_60;
      Continiue             : Boolean := True;
      Result                : Optimise.Optimise_Result;
   begin
      --  L266
      if not Bounds.Is_Empty then
         Assert (Positive (Bounds.Length) = X_Length, Routine_Name &
                   "Bounds and X have different lengths.");
      end if;

      for row in Bounds.First_Index .. Bounds.Last_Index loop
         null;
      end loop;

      --  L309

      Task_Name (1 .. 5) := ('S', 'T', 'A', 'R', 'T');
      --  L361
      while Continiue loop
         Lbfgsb_F_Interface.Setulb
           (M, X,
            Low_Bound, Upper_Bound, nbd, f, g, factor, pgtol,
            wa, iwa, Task_Name, I_Print, C_Save, L_Save, I_Save, D_Save,
            Fortran_Integer (Options.Max_Line_Steps));
      end loop;

      return Result;

   end Minimise_LBFGSB;

end LBFGSB;
