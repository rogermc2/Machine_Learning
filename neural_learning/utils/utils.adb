
--  Based on scikit-learn/sklearn/utils/__init__.py

--  with Ada.Text_IO; use Ada.Text_IO;

package body Utils is

   --  L695 Gen_Batches returns a list of slice lists
   function Gen_Batches (Num_To_Slice, Batch_Size : Positive;
                         Min_Batch_Size : Natural := 0)
                         return Slices_List is
--        Routine_Name : constant String := "Utils.Gen_Batches ";
      Start       : Positive := 1;
      Last        : Positive;
      aSlice      : Slice_Record;
      Slices      : Slices_List;
   begin
      for index in 1 .. Num_To_Slice / Batch_Size loop
         Last := Start + Batch_Size - 1;
         if Last + Min_Batch_Size <= Num_To_Slice then
            for item in Start .. Last loop
               aSlice := (Start, Last);
            end loop;
            Start := Last + 1;
         else
            for item in Start .. Num_To_Slice loop
               aSlice := (Start, Last);
            end loop;
         end if;
         Slices.Append (aSlice);
      end loop;

      return Slices;

   end Gen_Batches;

end Utils;
