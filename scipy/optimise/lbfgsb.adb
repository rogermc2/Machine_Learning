--  Based on scipy/optimize/_lbfgsb_py.py

with Ada.Assertions; use Ada.Assertions;

package body LBFGSB is

   function Minimise_LBFGSB (X : Real_Float_Matrix;
                              Bounds : Real_Float_Matrix)
                             return Optimise.Optimise_Result is
      Routine_Name : constant String := "LBFGSB.Minimise_LBFGSB";
      X_Length     : constant Positive := X'Length;
      X_Clip       : Real_Float_Matrix := X;
      Result       : Optimise.Optimise_Result;
   begin
      Assert (Bounds'Length = X_Length, Routine_Name &
                "Bounds and X have different lengths.");

      for row in X_Clip'Range loop
         for col in X_Clip'Range (2) loop
            if X_Clip (row, col) < Bounds (row, 1) then
              X_Clip (row, col) := Bounds (row, 1);
            elsif X_Clip (row, col) > Bounds (row, 2) then
               X_Clip (row, col) := Bounds (row, 2);
            end if;
         end loop;
      end loop;

      return Result;

   end Minimise_LBFGSB;

end LBFGSB;
