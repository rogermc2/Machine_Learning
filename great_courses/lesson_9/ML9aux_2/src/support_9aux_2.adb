
--  with Ada.Text_IO; use Ada.Text_IO;

with Maths;

--  with Basic_Printing; use Basic_Printing;
with ML_Types;

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
      type Data_List_Record is record
         Features : Float_Array_List;
         Labels   : ML_Types.Integer_List;
      end record;

      Data_Index : Natural := 0;
      Mini_Mask  : Boolean_Array (1 .. Data.Num_Train);
      Features   : Float_Array (1 .. Data.Num_Features);
      Data_List  : Data_List_Record;
   begin
      for index in Mini_Mask'Range loop
         Mini_Mask (index) :=
           Maths.Random_Integer (1, Data.Num_Train) <= MS_Size;
      end loop;

      for row in Data.Train_X'Range loop
         if Mini_Mask (row) then
            Data_Index := Data_Index + 1;
            for col in Data.Train_X'Range (2) loop
               Features (col) := Data.Train_X (row, col);
            end loop;
            Data_List.Features.Append (Features);
            Data_List.Labels.Append (Data.Train_Y (row, 1));
         end if;
      end loop;

      declare
         Features : Float_Array (1 .. Data.Num_Features);
         Result   : Data_Record (Positive (Data_List.Features.Length),
                                 Data.Num_Features);
      begin
         for row in 1 .. Data_List.Features.Length loop
            Features := Data_List.Features (Integer (row));
            for col in 1 .. Data.Num_Features loop
               Result.Features (Integer (row), col) := Features (col);
            end loop;

            Result.Labels (Integer (row), 1) :=
              Data_List.Labels (Integer (row));
         end loop;

         return Result;
      end;

   end Mini_Data;

   --  -------------------------------------------------------------------------

end Support_9AUX_2;
