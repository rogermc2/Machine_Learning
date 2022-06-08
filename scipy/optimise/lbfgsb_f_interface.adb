
package body Lbfgsb_F_Interface is

   function To_DP_Array (RA : Real_Float_Vector) return Fortran_DP_Array is
      Result : Fortran_DP_Array (RA'Range);
   begin
         for index in RA'Range loop
            Result (index) := Double_Precision (RA (index));
         end loop;

         return Result;
   end To_DP_Array;

end Lbfgsb_F_Interface;
