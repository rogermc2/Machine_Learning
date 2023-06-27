
with Ada.Assertions; use Ada.Assertions;
--  with Ada.Exceptions; use Ada.Exceptions;
--  with Ada.Text_IO; use Ada.Text_IO;

--  with Basic_Printing; use Basic_Printing;

package body Support_21A is

   function Dot_Trans_V (Trans : Trans_Tensor; Trans_Row : Positive;
                         V     : Integer_Matrix) return Integer_Matrix;
   function Product (L : Trans_Tensor; R : Integer_Matrix) return Trans_Tensor;

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
                      Grid_Map                     : Integer_Matrix) return Trans_Tensor is

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

      Rewards           : constant Integer_Array (Grid_Map'Range) :=
                            (0, -1, -1, -1, 10);
      Num_Acts          : constant Positive := 5;
      Rows_x_Cols       : constant Positive := Num_Rows * Num_Cols;
      Acts              : constant Integer_Matrix (1 .. Num_Acts, 1 ..2) :=
                            ((-1,0), (0,1), (1,0), (0,-1), (0,0));
      Mat_Map           : Trans_Tensor (Grid_Map'Range, Grid_Map'Range (2),
                                        1 .. Num_Cats);
      Mat_Trans         : Trans_Tensor (1 .. Num_Acts, 1 .. Rows_x_Cols,
                                        1 .. Rows_x_Cols) :=
                            (others => (others => (others => 0)));
      Q                 : Trans_Tensor (1 .. Num_Acts, 1 .. Rows_x_Cols,
                                        1 .. Rows_x_Cols);
      Q_Act             : Integer_Matrix (1 .. Rows_x_Cols, 1 .. Rows_x_Cols);
      rk                : Integer_Matrix (Grid_Map'Range, 1 .. 1);
      rfk               : Trans_Tensor (Grid_Map'Range, Grid_Map'Range (2), 1 .. 1);
      rffk              : Integer_Matrix (Grid_Map'Range, 1 .. 1);
      v                 : Integer_Matrix (Grid_Map'Range, 1 .. 1);
      Action            : Integer_Array (1 .. 2);
      Row_Next          : Positive;
      Col_Next          : Positive;
   begin
      for row in Mat_Map'Range loop
         for col in Mat_Map'Range (2) loop
            for cat in Mat_Map'Range (3) loop
               if Grid_Map (row, col) = cat then
                  Mat_Map (row, col, cat) := 1;
               else
                  Mat_Map (row, col, cat) := 0;
               end if;
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
                     if row_2 = Row_Next and col_2 = Col_Next then
                        Mat_Trans (acts_item, (row - 1) * Num_Cols + col,
                                   (row_2 - 1) * Num_Cols + col_2) := 1;
                     end if;
                  end loop;
               end loop;
            end loop;
         end loop;
      end loop;

      for row in Rewards'Range loop
         rk (row, 1) := Rewards (row);
      end loop;

      rfk := Product (Mat_Map, rk);

      for row in rfk'Range loop
         for col in rfk'Range (2) loop
            rffk ((row - 1) * rfk'Length (2) + col, 1) :=
              rfk (row, col, 1);
         end loop;
      end loop;
      v := rffk;

      for count in 1 .. 50 loop
         for act in Q'Range loop
            Q_Act := Dot_Trans_V (Mat_Trans, act, v);
            for row in Q_Act'Range loop
               for col in Q_Act'Range (2) loop
                  Q (act, row, col) := Q_Act (row, col);
               end loop;
            end loop;
         end loop;
      end loop;

      return Mat_Trans;

   end Binarize;

   --  -------------------------------------------------------------------------

   function Dot_Trans_V (Trans : Trans_Tensor; Trans_Row : Positive;
                         V     : Integer_Matrix) return Integer_Matrix is
      Act    : Integer_Matrix (Trans'Range (2), Trans'Range (3));
   begin
      for row in Act'Range loop
         for col in Act'Range (2) loop
            Act (row, col) := Trans (Trans_Row, row, col);
         end loop;
      end loop;

      return Dot (Act, V);

   end Dot_Trans_V;

   --  -------------------------------------------------------------------------
   --  for matrix A of dimensions (m,n,p) and B of dimensions (p,s)
   --  C(i, j, k) = sum[r=1 to p] A(i, j, r) * B(r, k)
   function Product (L : Trans_Tensor; R : Integer_Matrix) return Trans_Tensor is
      Routine_Name : constant String := "Support_21A.Dot ";
      Sum          : Integer;
      Result       : Trans_Tensor (L'Range, L'Range (2), R'Range (2));
   begin
      Assert (R'Length (2) = L'Length (3), Routine_Name &
                "R'LenResultgth (2) not = L'Length (3)");
      for li in L'Range loop
         for lj in L'Range (2) loop
            for rk in R'Range (2) loop
               Sum := 0;
               for lr in L'Range (3) loop  -- r
                  --  Result(i, j, k) = sum (L(i, j, r) * R(r, k))
                  Sum := Sum + L(li, lj, lr) * R (lr, rk);
                  Assert (Sum'Valid, Routine_Name & "Sum =" &
                            Integer'Image (Sum) & "row, col:" &
                            Integer'Image (li) & ", " & Integer'Image (lj) &
                            ", L, R:" & Integer'Image (L (li, lj, lr)) &
                            ", " & Integer'Image (R (lr, rk)));
               end loop;
               Result (li, lj, rk) := Sum;
            end loop;
         end loop;
      end loop;

      return Result;

   end Product;

   --  ----------------------------------------------------------------------------

end Support_21A;
