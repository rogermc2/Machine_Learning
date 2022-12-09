
with Ada.Assertions; use Ada.Assertions;
--  with Ada.Text_IO; use Ada.Text_IO;

--  with Printing;

package body Plot_Utilities is

   use ML_Types;

   --  -------------------------------------------------------------------------

   function To_PL_Array (List_1D : ML_Types.Value_Data_List; Num_Rows : Positive)
                         return PLplot_Auxiliary.Real_Matrix is
      Routine_Name : constant String :=
                       "Classifier_Utilities.To_PL_Array ";
      Length_1D    : constant Positive := Positive (List_1D.Length);
      Num_Cols     : constant Positive := Length_1D / Num_Rows;
      End_Offset   : constant Positive := Num_Cols - 1;
      Start        : Positive := List_1D.First_Index;
      Result       : PLplot_Auxiliary.Real_Matrix (1 .. Num_Rows, 1 .. Num_Cols);
   begin
      Assert (Num_Rows * Num_Cols = Length_1D, Routine_Name & "Num_Rows" &
                Integer'Image (Num_Rows) & " is incompatible with List_1D size"
              & Integer'Image (Length_1D));

      for row in reverse 1 .. Num_Rows loop
         for col in Start .. Start + End_Offset loop
            Result (col - Start + 1, row) := Long_Float (List_1D.Element (col).Float_Value);
         end loop;
         Start := Start + Num_Cols;
      end loop;

      return Result;

   end To_PL_Array;

   --  -------------------------------------------------------------------------

end Plot_Utilities;
