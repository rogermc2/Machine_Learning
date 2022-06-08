
with Interfaces.Fortran; use Interfaces.Fortran;

with NL_Arrays_And_Matrices; use NL_Arrays_And_Matrices;

package Lbfgsb_F_Interface is

   type Fortran_DP_Array is array (Integer range <>) of Double_Precision;
   pragma Convention (Fortran, Fortran_DP_Array);

   type Fortran_Integer_Array is array (Integer range <>) of Integer;
   pragma Convention (Fortran, Fortran_Integer_Array);

   type Fortran_LSave_Array is array (Integer range 1 .. 4) of Integer;
   pragma Convention (Fortran, Fortran_LSave_Array);

   type Fortran_ISave_Array is array (1 .. 44) of Integer;
   pragma Convention (Fortran, Fortran_ISave_Array);

   type Fortran_DSave_Array is array (Integer range 1 .. 29) of
     Double_Precision;
   pragma Convention (Fortran, Fortran_DSave_Array);

   type Character_60 is new Fortran_Character (1 .. 60);

   procedure setulb (m    : in Fortran_Integer;
                     x    : in out Fortran_DP_Array;
                     l, u : in Fortran_DP_Array;
                     nbd  : in Fortran_Integer_Array;
                     f    : in out Double_Precision;
                     g            : in out Fortran_DP_Array;
                     factr, pgtol : in out Double_Precision;
                     wa           : in out Fortran_DP_Array;
                     iwa          : in out Fortran_Integer_Array;
                     TaskName     : in out Character_60;
                     iprint       : in Fortran_Integer;
                     csave : in out Character_60;
                     lsave : in out Fortran_LSave_Array;
                     isave : in out Fortran_Integer_Array;
                     dsave : in out Fortran_DSave_Array;
                     maxls : in Fortran_Integer);
   pragma Import (Fortran, setulb);

   function To_DP_Array (RA : Real_Float_Vector) return Fortran_DP_Array;

end Lbfgsb_F_Interface;
