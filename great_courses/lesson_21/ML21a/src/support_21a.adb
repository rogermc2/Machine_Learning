
--  with Ada.Assertions; use Ada.Assertions;
--  with Ada.Exceptions; use Ada.Exceptions;
--  with Ada.Text_IO; use Ada.Text_IO;

--  with Basic_Printing; use Basic_Printing;

package body Support_21A is

   --  -------------------------------------------------------------------------
   --  Arg_Max returns the index from indices associated with the item in the
   --  Values list with the highest value.
   --     function Arg_Max (Indices : Integer_Array; Values : Real_Float_Vector)
   --                       return Integer is
   --        --        Routine_Name : constant String := "Support_16A.Arg_Max ";
   --        Best_Index   : Positive;
   --        pragma Warnings (Off);
   --        Best         : constant Float := Max (Values, Best_Index);
   --        pragma Warnings (On);
   --     begin
   --        return Indices (Best_Index);
   --
   --     end Arg_Max;
   --     pragma Inline (Arg_Max);

   --  -------------------------------------------------------------------------
   --  Binarize grid_map, adding an additional dimension representing the value
   --  in the cell to make matmap.
   --  Values in matmap equal 1 if the value of row and column of that cell in
   --  grid_map equal is the value of the third dimension of the cell.
   --  Clip keeps the current location in the grid to be within the size of the grid.
   function Binarize (Num_Rows, Num_Cols, Num_Cats : Positive;
                      Grid_Map : Integer_Matrix) return Trans_Array is

      function Clip (Val, Min, Max : Integer) return Integer is
         Result : Integer := Val;
      begin
         if Result < Min then
            Result := Min;
         end if;

         if Result > Max - 1 then
            Result := Max - 1;
         end if;

         return Result;

      end Clip;

      Rewards   : constant Integer_Array (1 .. 5) := (0, -1, -1, -1, 10);
      Num_Acts  : constant Positive := 5;
      Acts      : constant Integer_Matrix (1 .. Num_Acts, 1 ..2) :=
        ((-1,0), (0,1), (1,0), (0,-1), (0,0));
      Mat_Map   : array (1 .. Num_Rows, 1 .. Num_Cols, 1 .. Num_Cats) of Boolean;
      Mat_Trans : Trans_Array (1 .. Num_Acts, 1 .. Num_Rows * Num_Cols,
                               1 .. Num_Rows * Num_Cols);
      Action    : Integer_Array (1 .. 2);
      Row_Next  : Positive;
      Col_Next  : Positive;
   begin
      for row in Mat_Map'Range loop
         for col in Mat_Map'Range (2) loop
            for cat in Mat_Map'Range (3) loop
               Mat_Map (row, col, cat) := Grid_Map (row, col) = cat;
            end loop;
         end loop;
      end loop;

      for acts_item in Acts'Range loop
         Action := Get_Row (Acts, acts_item);
         for row in Mat_Map'Range  loop
            for col in Mat_Map'Range (2) loop
               Row_Next := Clip (row + Action (1) + 1, 1, Num_Rows);
               Col_Next := Clip (col + Action (2) + 1, 1, Num_Cols);
               for row_2 in Mat_Map'Range  loop
                  for col_2 in Mat_Map'Range (2) loop
                     Mat_Trans (acts_item, (row - 1) * Num_Cols + col,
                                (row_2 - 1) * Num_Cols + col_2) :=
                       row_2 = Row_Next and col_2 = Col_Next;
                  end loop;
               end loop;
            end loop;
         end loop;
      end loop;

      return Mat_Trans;

   end Binarize;

   --  -------------------------------------------------------------------------

end Support_21A;
