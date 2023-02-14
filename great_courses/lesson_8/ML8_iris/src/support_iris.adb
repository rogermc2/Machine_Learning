
with Ada.Text_IO; use Ada.Text_IO;

--  with Basic_Printing; use Basic_Printing;

package body Support_Iris is

   --  -------------------------------------------------------------------------

   function Test_Score (Predictions : Real_Float_Vector;
                        Labels      : Integer_Matrix) return Natural is
      Routine_Name : constant String := "Support_8A.Test_Score ";
      Correct      : Natural := 0;
      Incorrect    : Natural := 0;
   begin
      for index in Predictions'Range loop
         if Integer (Predictions (index)) = Labels (index, 1) then
            Correct := Correct + 1;
         else
            Incorrect := Incorrect + 1;
            if Incorrect < 10 then
               Put_Line (Routine_Name & "incorrect prediction:" &
                           Float'Image (Predictions (index))  &  " for " &
                           Integer'Image (Labels (index, 1)));
            end if;
         end if;
      end loop;

      Put_Line (Routine_Name & "correct predictions:" &
                  Integer'Image (Correct));
      Put_Line (Routine_Name & "incorrect predictions:" &
                  Integer'Image (Incorrect));

      return Correct;

   end Test_Score;

   --  -------------------------------------------------------------------------

end Support_Iris;
