--  Based on scipy/scipy/optimize/lbfgsb_src/lbfgsb.f

with Interfaces.Fortran; use Interfaces.Fortran;

with Ada.Text_IO; use Ada.Text_IO;

package body Lbfgsb_F_Interface is

   type Fortran_DP_Array is array (Integer range <>) of Double_Precision;
   pragma Convention (Fortran, Fortran_DP_Array);

   type Fortran_Integer_Array is array (Integer range <>) of Fortran_Integer;
   pragma Convention (Fortran, Fortran_Integer_Array);

   type Fortran_DSave_Array is array (Integer range 1 .. 29) of
     Double_Precision;
   pragma Convention (Fortran, Fortran_DSave_Array);

   type Character_60 is new Fortran_Character (1 .. 60);

   Csave  : Character_60;
   Dsave  : Fortran_DSave_Array;

   function To_Ada (IA : Fortran_Integer_Array) return Integer_Array;
   function To_DP_Array (RA : Real_Float_Vector) return Fortran_DP_Array;
   function To_Fortran (IA : Integer_Array) return Fortran_Integer_Array;
   function To_RF_Array (DPA : Fortran_DP_Array) return Real_Float_Vector;

   --  -------------------------------------------------------------------------

   --  x is a double precision array of dimension n.
   --  On entry x is an approximation to the solution.
   --  On exit x is the current approximation.
   --  f is a double precision variable.
   --  On first entry f is unspecified.
   --  On final exit f is the value of the function at x.
   --  g is a double precision array of dimension n.
   --  On first entry g is unspecified.
   --  On final exit g is the value of the gradient at x.
   --  nbd is an integer array of dimension n.
   --  On entry nbd represents the type of bounds imposed on the
   --  variables and must be specified by:
   --  nbd(i)=0 if x(i) is unbounded,
   --         1 if x(i) has only a lower bound,
   --         2 if x(i) has both lower and upper bounds, and
   --         3 if x(i) has only an upper bound.
   --  factr is a double precision variable specified on entry as factr >= 0
   --  The iteration stops when
   --     (f^k - f^{k+1})/max{|f^k|,|f^{k+1}|,1} <= factr*epsmch
   --     where epsmch is the machine precision, automatically
   --     generated by the code.
   --  Typical values for factr: 10 ** 12 for low accuracy;
   --  10 ** 7 for moderate accuracy and 10 for extremely high accuracy.
   --  wa is a double precision working array of length
   --  (2mmax + 5)nmax + 12mmax^2 + 12mmax.
   --  iwa is an integer working array of length 3nmax.
   --  isave is an integer working array of dimension 44.
   --  On exit with 'task' = NEW_X, the following information is                                                       available:
   --  isave(22) = the total number of intervals explored in the search of
   --              Cauchy points;
   --  isave(26) = the total number of skipped BFGS updates before the current
   --              iteration;
   --  isave(30) = the number of the current iteration;
   --  isave(31) = the total number of BFGS updates prior to the current
   --              iteration;
   --  isave(33) = the number of intervals explored in the search for a Cauchy
   --              point in the current iteration;
   --  isave(34) = the total number of function and gradient evaluations;
   --  isave(36) = the number of function value or gradient evaluations in the
   --              current iteration;
   --  if isave(37) = 0  then the subspace argmin is within the box;
   --  if isave(37) = 1  then the subspace argmin is beyond the box;
   --  isave(38) = the number of free variables in the current iteration;
   --  isave(39) = the number of active constraints in the current iteration;
   --  n + 1 - isave(40) = the number of variables leaving the set of
   --                       active constraints in the current iteration;
   --  isave(41) = the number of variables entering the set of active
   --              constraints in the current iteration.

   procedure setulb (n            : in Fortran_Integer;
                     m            : in Fortran_Integer;
                     x            : in out Fortran_DP_Array;
                     l, u         : in Fortran_DP_Array;  --  bounds
                     nbd          : in Fortran_Integer_Array;
                     f            : in out Double_Precision;
                     g            : in out Fortran_DP_Array;
                     factr, pgtol : in out Double_Precision;
                     wa           : in out Fortran_DP_Array;
                     iwa          : in out Fortran_Integer_Array;
                     TaskName     : in out Character_60;
                     iprint       : in Fortran_Integer := -1;
                     csave        : in out Character_60;
                     lsave        : in out Fortran_Integer_Array;
                  isave        : in out Fortran_Integer_Array;
                     dsave        : in out Fortran_DSave_Array;
                     maxls        : in Fortran_Integer);
   pragma Import (Fortran, setulb);

   --  -------------------------------------------------------------------------

   procedure Set_Ulb (SN               : Integer;
                      SM               : Integer;
                      SX               : in out Real_Float_Vector;
                      SL, SU           : Real_Float_Vector;  --  bounds
                      S_Nbd            : Integer_Array;
                      SF               : in out Float;
                      SG               : in out Real_Float_Vector;
                      S_Factr, S_Pgtol : in out Float;
                      S_Wa             : in out Real_Float_Vector;
                      S_Iwa            : in out Integer_Array;
                      S_Task_Name      : in out Unbounded_String;
                      S_Lsave          : in out LSave_Array;
                      S_Isave          : in out Integer_Array;
                      S_Maxls          : Integer) is
      Routine_Name : constant String := "Lbfgsb_F_Interface.Set_Ulb ";
      X            : Fortran_DP_Array := To_DP_Array (SX);
      F            : Double_Precision := Double_Precision (SF);
      G            : Fortran_DP_Array := To_DP_Array (SG);
      Factr        : Double_Precision := Double_Precision (S_Factr);
      Pgtol        : Double_Precision := Double_Precision (S_Pgtol);
      Wa           : Fortran_DP_Array := To_DP_Array (S_Wa);
      Iwa          : Fortran_Integer_Array := To_Fortran (S_Iwa);
      Task_String  : constant String := To_String (S_Task_Name);
      Task_Name    : Character_60 := (others => To_Fortran (' '));
      Lsave        : Fortran_Integer_Array :=
                         To_Fortran (Integer_Array (S_Lsave));
      isave        : Fortran_Integer_Array := To_Fortran (S_Isave);
      NBD          : Fortran_Integer_Array := To_Fortran (S_Nbd);
   begin
      for index in Task_String'Range loop
         Task_Name (index) := To_Fortran (Task_String (index));
      end loop;

      for index in NBD'Range loop
         NBD (index) := Fortran_Integer (S_Nbd (index));
      end loop;

--        Put_Line (Routine_Name & "X in:");
--        for index in X'First .. X'First + 3 loop
--           Put (Integer'Image (index) & ":" &
--                  Double_Precision'Image (X (index)) & "  ");
--        end loop;
--        New_Line;

--  subroutine setulb(n, m, x, l, u, nbd, f, g, factr, pgtol, wa, iwa,
--                    task, iprint, csave, lsave, isave, dsave, maxls)
      setulb (n        => Fortran_Integer (SN),
              m        => Fortran_Integer (SM),
              x        => X,
              l        => To_DP_Array (SL),
              u        => To_DP_Array (SU),
              nbd      => NBD,
              f        => F,
              g        => G,
              factr    => Factr,
              pgtol    => Pgtol,
              wa       => Wa,
              iwa      => Iwa,
              TaskName => Task_Name,
              iprint   => Fortran_Integer (-1),
              csave    => Csave,
              lsave    => Lsave,
           isave    => Isave,
              dsave    => Dsave,
              maxls    => Fortran_Integer (S_Maxls));

      Put_Line (Routine_Name & "Task_Name out:" & To_Ada (Task_Name));
      Put_Line (Routine_Name & "X out:");
      for index in X'First .. X'First + 3 loop
         Put (Double_Precision'Image (X (index)) & "  ");
      end loop;
      New_Line;

      SX := To_RF_Array (X);
      SF := Float (F);
      SG := To_RF_Array (G);
      S_Factr := Float (Factr);
      S_Pgtol := Float (Pgtol);
      S_Wa := To_RF_Array (Wa);
      S_Iwa := To_Ada (Iwa);
      S_Task_Name := Trim (To_Unbounded_String (To_Ada (Task_Name)),
                           Ada.Strings.Right);
      S_Lsave := LSave_Array (To_Ada (Lsave));
      S_Isave := To_Ada (Isave);
      Put_Line (Routine_Name & "Isave (1)" & Fortran_Integer'Image (Isave (1)));
      Put_Line (Routine_Name & "Isave (31)" & Fortran_Integer'Image (Isave (31)));

   end Set_Ulb;

   --  -------------------------------------------------------------------------

   function To_DP_Array (RA : Real_Float_Vector) return Fortran_DP_Array is
      Result : Fortran_DP_Array (RA'Range);
   begin
      for index in RA'Range loop
         Result (index) := Double_Precision (RA (index));
      end loop;

      return Result;
   end To_DP_Array;

   --  -------------------------------------------------------------------------

   function To_Ada (IA : Fortran_Integer_Array) return Integer_Array is
      Result : Integer_Array (IA'Range);
   begin
      for index in IA'Range loop
         Result (index) := Integer (IA (index));
      end loop;

      return Result;

   end To_Ada;

   --  -------------------------------------------------------------------------

   function To_Fortran (IA : Integer_Array) return Fortran_Integer_Array is
      Result : Fortran_Integer_Array (IA'Range);
      pragma Convention (Fortran, Result);
   begin
      for index in IA'Range loop
         Result (index) := Fortran_Integer (IA (index));
      end loop;

      return Result;

   end To_Fortran;

   --  -------------------------------------------------------------------------

   function To_RF_Array (DPA : Fortran_DP_Array) return Real_Float_Vector is
      Result : Real_Float_Vector (DPA'Range);
   begin
      for index in DPA'Range loop
         Result (index) := Float (DPA (index));
      end loop;

      return Result;

   end To_RF_Array;

   --  -------------------------------------------------------------------------

end Lbfgsb_F_Interface;
