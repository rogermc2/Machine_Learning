--  Based on scipy/scipy/optimize/lbfgsb_src/lbfgsb.f

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with NL_Arrays_And_Matrices; use NL_Arrays_And_Matrices;

package Lbfgsb_F_Interface is

   subtype S60 is String (1 .. 60);
   type LSave_Array is array (Integer range 1 .. 4) of Integer;

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
--                        S_Csave          : in out S60;
                      S_Task_Name      : in out Unbounded_String;
                      S_Lsave          : in out LSave_Array;
                      S_Isave          : in out Integer_Array;
                      S_Maxls          : Integer);

end Lbfgsb_F_Interface;
