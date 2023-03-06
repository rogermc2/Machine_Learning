
package body Support_9AUX_2 is

   --  -------------------------------------------------------------------------

   function Error (Predictions : Real_Float_Vector;
                   Labels      : Integer_Matrix) return Float is
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

   function Mini_Data (Data    : CSV_Data_Loader.Base_Split_State;
                       MS_Size : Positive) return Data_Record is
      Data_Index       : Natural := 0;
      Mini_Mask        : Boolean_Array (1 .. Data.Num_Train);
   begin
      for index in Mini_Mask'Range loop
         Mini_Mask (index) :=
           Integer (Float'Ceiling (Float (index * Data.Num_Train) / 10.0))
           <= MS_Size;
      end loop;

      declare
         Result : Data_Record (MS_Size, Data.Num_Features);
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
