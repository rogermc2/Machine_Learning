--  Based on scikit-learn/sklearn/utils/optimise.py

with Ada.Text_IO; use Ada.Text_IO;

package body Utils_Optimise is

   function Check_Optimize_Result (Result : Optimise.Optimise_Result;
                                   Max_Iter : Natural := 0) return Natural is
      Routine_Name : constant String := "Utils_Optimise.Check_Optimize_Result ";
      Iter : Natural;
   begin
      if not Result.Success then
         Put_Line (Routine_Name & "WARNING: Solver failed to converge." &
                  " Increase the number of iterations (Max_Iter) " &
                "or scale the data as shown in:  " &
                "https://scikit-learn.org/stable/modules/preprocessing.html");
      end if;

      if Max_Iter > 0 then
         Iter := Integer'Min (Result.N_It, Max_Iter);
      else
         Iter := Result.N_It;
      end if;

      return Iter;

   end Check_Optimize_Result;

end Utils_Optimise;
