
--  with Ada.Text_IO; use Ada.Text_IO;

with Maths;

--  with Basic_Printing; use Basic_Printing;

package body Support_9AUX_2 is

   --  -------------------------------------------------------------------------

   function Error (Predictions : Real_Float_Vector;
                   Labels      : Integer_Matrix) return Float is
      --        Routine_Name : constant String := "Support_9AUX_2.Error ";
      Incorrect    : Natural := 0;
   begin
      for index in Predictions'Range loop
         if Integer (Predictions (index)) /= Labels (index, 1) then
            Incorrect := Incorrect + 1;
         end if;
      end loop;

      return Float (Incorrect) / Float (Labels'Length);

   end Error;

   --  -------------------------------------------------------------------------

   function Init_MS (Length : Positive) return Integer_Array is
      MS  : Integer_Array (1 .. 10);
   begin
      for index in MS'Range loop
         MS (index) :=
           Integer (Float'Ceiling (Float (index * Length) / 10.0));
      end loop;

      return MS;

   end Init_MS;

   --  -------------------------------------------------------------------------

   function Mini_Data (Data : CSV_Data_Loader.Base_Split_State; MS : Positive)
                       return Data_Record is
      Data_Length      : Natural := 0;
      Data_Index       : Natural := 0;
      Mini_Mask        : Boolean_Array (1 .. Data.Num_Train);
   begin
      for index in Mini_Mask'Range loop
         Mini_Mask (index) :=
           Integer (Float'Ceiling (Float (index * Data.Num_Train) / 10.0))
           <= MS;
      end loop;

      for mask_index in Mini_Mask'Range loop
         Mini_Mask (mask_index) :=
           Maths.Random_Integer (1, Data.Num_Train) <= mask_index;
         if Mini_Mask (mask_index) then
            Data_Length := Data_Length + 1;
         end if;
      end loop;

      declare
         Result : Data_Record (Data_Length, Data.Num_Features);
      begin
         for row in Data.Train_X'Range loop
            if Mini_Mask (row) then
               Data_Index := Data_Index + 1;
               for col in Data.Train_X'Range (2) loop
                  Result.Features (Data_Index, col) := Data.Train_X (row, col);
                  Result.Labels (Data_Index, 1) := Data.Train_Y (row, 1);
               end loop;
            end if;
         end loop;

         return Result;
      end;

   end Mini_Data;

   --  -------------------------------------------------------------------------

end Support_9AUX_2;
