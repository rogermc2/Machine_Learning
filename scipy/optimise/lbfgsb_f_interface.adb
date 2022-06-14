
package body Lbfgsb_F_Interface is

   function To_DP_Array (RA : Real_Float_Vector) return Fortran_DP_Array is
      Result : Fortran_DP_Array (RA'Range);
   begin
         for index in RA'Range loop
            Result (index) := Double_Precision (RA (index));
         end loop;

         return Result;
   end To_DP_Array;

   --  -------------------------------------------------------------------------

   function Zero_Array (Num_Rows : Positive) return Fortran_DP_Array is
      Result : Fortran_DP_Array (1 .. Num_Rows);
   begin
         for index in Result'Range loop
            Result (index) := Double_Precision (0.0);
         end loop;

         return Result;
   end Zero_Array;

   --  -------------------------------------------------------------------------

end Lbfgsb_F_Interface;
