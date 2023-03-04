
--  with Ada.Text_IO; use Ada.Text_IO;

--  with Basic_Printing; use Basic_Printing;

package body Support_9AUX is

   --  -------------------------------------------------------------------------

   function Error (Predictions : Real_Float_Vector;
                   Labels      : Integer_Matrix) return Float is
--        Routine_Name : constant String := "Support_9AUX.Error ";
      Incorrect    : Natural := 0;
   begin
--        Print_Float_Vector ("Predictions", Predictions, 100, 110);
--        Print_Integer_Matrix ("Labels", Labels, 100, 110);
      for index in Predictions'Range loop
         if Integer (Predictions (index)) /= Labels (index, 1) then
            Incorrect := Incorrect + 1;
         end if;
      end loop;

      return Float (Incorrect) / Float (Labels'Length);

   end Error;

   --  -------------------------------------------------------------------------

end Support_9AUX;
