
--  Based on scikit-learn/sklearn/utils/__init__.py

--  with Ada.Text_IO; use Ada.Text_IO;

package body Utils is

   --  L695
   function Gen_Batches (Num_To_Slice, Batch_Size : Positive;
                         Min_Batch_Size : Natural := 0)
                         return IL_Types.Integer_List_2D is
      use IL_Types;
--        Routine_Name : constant String := "Utils.Gen_Batches ";
      Start       : Positive := 1;
      Last        : Positive;
      aSlice      : Integer_List;
      Slices      : Integer_List_2D;
   begin
      for index in 1 .. Num_To_Slice / Batch_Size loop
         Last := Start + Batch_Size - 1;
         aSlice.Clear;
         if Last + Min_Batch_Size <= Num_To_Slice then
            for item in Start .. Last loop
               aSlice.Append (item);
            end loop;
            Start := Last + 1;
         else
            for item in Start .. Num_To_Slice loop
               aSlice.Append (item);
            end loop;
         end if;
         Slices.Append (aSlice);
      end loop;

      return Slices;

   end Gen_Batches;

end Utils;
