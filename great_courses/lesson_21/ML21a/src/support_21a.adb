
with Ada.Assertions; use Ada.Assertions;
--  with Ada.Exceptions; use Ada.Exceptions;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded;

with Basic_Printing; use Basic_Printing;
--  with Python_21a;

package body Support_21A is

   function Compute_Q (Mat_Trans : Binary_Tensor; v : Real_Float_Matrix;
                       Num_Acts : Integer) return Real_Float_Matrix;
   function Dot_Trans_V (Trans : Binary_Tensor; Trans_Row : Positive;
                         V     : Real_Float_Matrix) return Real_Float_Matrix;
   function Pi_Max (Pi : Real_Float_Matrix; Row : Positive) return Float;
   function Product (L : Binary_Tensor; R : Integer_Matrix) return Integer_Tensor;
   --     function Sum_Each_Column (Data : Float_Tensor) return Real_Float_Matrix;

   --  -------------------------------------------------------------------------

   --     function "*" (B : Float; Q : Trans_Tensor) return Float_Tensor is
   --        Result : Float_Tensor (Q'Range, Q'Range (2), Q'Range (3));
   --     begin
   --        for row in Q'Range loop
   --           for col in Q'Range (2) loop
   --              for item in Q'Range (3) loop
   --                 Result (row, col, item) := B * Float (Q (row, col, item));
   --              end loop;
   --           end loop;
   --        end loop;
   --
   --        return Result;
   --
   --     end "*";

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
   function Binarize (Classifier : Python.Module;
                      Num_Rows, Num_Cols, Num_Cats : Positive;
                      Grid_Map                     : Integer_Matrix)
                      return Binary_Tensor is
      use Real_Float_Arrays;
      Routine_Name : constant String := "Support_21A.Binarize ";

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
      Beta              : constant Float := 10.0;
      Gamma             : constant Float := 0.9;
      Mat_Map           : Binary_Tensor (Grid_Map'Range, Grid_Map'Range (2),
                                         1 .. Num_Cats);
      Mat_Trans         : Binary_Tensor (1 .. Num_Acts, 1 .. Rows_x_Cols,
                                         1 .. Rows_x_Cols) :=
        (others => (others => (others => 0)));
      Q                 : Real_Float_Matrix (1 .. Rows_x_Cols, 1 .. Num_Acts);
      --        Q_Act             : Real_Float_Matrix (1 .. Num_Cols, 1 .. Num_Acts);
      rk                : Integer_Matrix (Grid_Map'Range, 1 .. 1);
      rfk               : Integer_Tensor (Grid_Map'Range, Grid_Map'Range (2), 1 .. 1);
      rffk              : Real_Float_Matrix (1 .. Rows_x_Cols, 1 .. 1);
      v                 : Real_Float_Matrix (1 .. Rows_x_Cols, 1 .. 1);
      Action            : Integer_Array (1 .. 2);
      Row_Next          : Positive;
      Col_Next          : Positive;
      Pi                : Real_Float_Matrix (1 .. Num_Rows, 1 .. Num_Cols);
      Pi_Q              : Real_Float_Matrix (Q'Range, Q'Range (2));
      Pi_Q_Sum          : Real_Float_Vector (Q'Range (2));
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
              Float (rfk (row, col, 1));
         end loop;
      end loop;
      v := rffk;

      for count in 1 .. 50 loop
         Q := Compute_Q (Mat_Trans, v, Num_Acts);

         Pi := Python.Call (Classifier, "softmax", Beta * Q);
         Pi_Q := Pi * Q;
         Pi_Q_Sum := Sum_Each_Column (Pi_Q);
         v := rffk + gamma * Pi_Q_Sum;
      end loop;

      Put_Line (Routine_Name & "Plot_Policy");
      Plot_Policy (Pi, Acts, Num_Rows, Num_Cols);
      Put_Line (Routine_Name & "Policy plotted");

      return Mat_Trans;

   end Binarize;

   --  -------------------------------------------------------------------------

   function Compute_Q (Mat_Trans : Binary_Tensor; v : Real_Float_Matrix;
                       Num_Acts  : Integer) return Real_Float_Matrix is
      Routine_Name : constant String := "Support_21A.Compute_Q ";
      Q            : Real_Float_Matrix (Mat_Trans'Range (2), Mat_Trans'Range);
      Index        : Natural;
      Q_Row        : Positive;
   begin
      Put_Line (Routine_Name & "Mat_Trans dim:" &
                  Integer'Image (Mat_Trans'Length) &
                  Integer'Image (Mat_Trans'Length (2)) &
                  Integer'Image (Mat_Trans'Length (3)));
      Print_Matrix_Dimensions (Routine_Name & "Q", Q);
      for act_index in Natural range 1 .. Num_Acts loop
         Put_Line (Routine_Name & "act_index, Num_Acts:" &
                     Integer'Image (act_index) & Integer'Image (Num_Acts));
         Index := act_index;
         declare
            Q_Act : constant Real_Float_Matrix :=
              Dot_Trans_V (Mat_Trans, Index, v);
         begin
            --           Print_Matrix_Dimensions (Routine_Name & "Q_Act", Q_Act);
            Put_Line (Routine_Name & "act_index:" & Integer'Image (act_index));
            for row in Q_Act'Range loop
               for col in Q_Act'Range (2) loop
                  Q_Row := (act_index - 1) * col + row;
                  Put_Line (Routine_Name & "Q_Row:" & Integer'Image (Row));
--                    Put_Line (Routine_Name & "(act_index - 1) * col + row, col:" &
--                                Integer'Image ((act_index - 1) * col + row ) &
--                                Integer'Image (col));
--                    Put_Line (Routine_Name & "Q_Act (row, col):" &
--                                Float'Image (Q_Act (row, col)));
--                    Q ((act_index - 1) * col + row, col) := Q_Act (row, col);
                  Put_Line (Routine_Name & "Q_Row, col:" & Integer'Image (Q_Row) &
                              Integer'Image (col));
                  Put_Line (Routine_Name & "Q (Q_Row, col):" & Float'Image (Q (Q_Row, col)));
               end loop;
            end loop;
         end;
         Put_Line (Routine_Name & "after declare block, act_index:" & Integer'Image (act_index));
      end loop;
      Put_Line (Routine_Name & "done");

      return Q;

   end Compute_Q;

   --  -------------------------------------------------------------------------

   function Dot_Trans_V (Trans : Binary_Tensor; Trans_Row : Positive;
                         V     : Real_Float_Matrix) return Real_Float_Matrix is
      use Real_Float_Arrays;
      Act    : Real_Float_Matrix (Trans'Range (2), Trans'Range (3));
   begin
      for row in Act'Range loop
         for col in Act'Range (2) loop
            Act (row, col) := Float (Trans (Trans_Row, row, col));
         end loop;
      end loop;

      return Act * V;

   end Dot_Trans_V;

   --  -------------------------------------------------------------------------

   procedure Find_Policy (Grid : in out Integer_Matrix; Pi : Real_Float_Matrix;
                          Acts : Integer_Matrix; Row_In, Col_In : Positive) is
      Num_Cols : constant Positive := Grid'Length (2);
      Row      : Positive := Row_In;
      Col      : Positive := Col_In;
      Pi_Row   : Positive;
      Max_Prob : Float;
      A        : Positive := 6;
   begin
      while Grid (Row, Col) = 6 loop
         Pi_Row := (Row - 1) * Num_Cols;
         Max_Prob := Pi_Max (Pi, Pi_Row);
         for ana in 1 .. 5 loop
            if Pi (Pi_Row, ana) = Max_Prob then
               A := ana;
            end if;
         end loop;
         Grid (Row, Col) := A;
         Row := Row + Acts (A, 1);
         Col := Col + Acts (A, 2);
      end loop;
      Find_Policy (Grid, Pi, Acts, Row, Col);

   end Find_Policy;

   --  -------------------------------------------------------------------------

   function Pi_Max (Pi : Real_Float_Matrix; Row : Positive) return Float is
      Result : Float := Pi (Row, 1);
   begin
      for col in Pi'Range (2) loop
         if Pi (Row, col) > Result then
            Result := Pi (Row, col);
         end if;
      end loop;

      return Result;

   end Pi_Max;

   --  -------------------------------------------------------------------------

   procedure Plot_Policy (Pi : Real_Float_Matrix; Acts : Integer_Matrix;
                          Num_Rows, Num_Cols : Positive) is
      use Ada.Strings.Unbounded;
      Grid : Integer_Matrix (1 .. Num_Rows, 1 .. Num_Cols) := (others => (others => 6));
      Line : Unbounded_String;
   begin
      Find_Policy (Grid, Pi, Acts, 1, 1);

      for row in 1 .. Num_Rows loop
         for col in 1 .. Num_Cols loop
            Line := Line & "^>v<x? " & To_Unbounded_String (Integer'Image (Grid (row, col)));
         end loop;
         Put_Line (To_String (Line));
      end loop;

   end Plot_Policy;

   --  -------------------------------------------------------------------------

   --  for matrix L of dimensions (m,n,p) and R of dimensions (p,s)
   --  C(i, j, k) = sum[r=1 to p] L(i, j, r) * R(r, k)
   function Product (L : Binary_Tensor; R : Integer_Matrix) return Integer_Tensor is
      Routine_Name : constant String := "Support_21A.Product ";
      Sum          : Integer;
      Result       : Integer_Tensor (L'Range, L'Range (2), R'Range (2));
   begin
      Assert (R'Length = L'Length (3), Routine_Name &
                "R'Length not = L'Length (3)");
      for li in L'Range loop
         for lj in L'Range (2) loop
            for rk in R'Range (2) loop
               Sum := 0;
               for rr in L'Range (3) loop  -- r
                  --  Result(i, j, k) = sum (L(i, j, r) * R(r, k))
                  Sum := Sum + L(li, lj, rr) * R (rr, rk);
               end loop;
               Result (li, lj, rk) := Sum;
            end loop;
         end loop;
      end loop;

      return Result;

   end Product;

   --  ----------------------------------------------------------------------------

   --     function Sum_Each_Column (Data : Float_Tensor) return Real_Float_Matrix is
   --        Sum    : Float;
   --        Result : Real_Float_Matrix (Data'Range (2), Data'Range (3));
   --     begin
   --        for col in Data'Range (2) loop
   --           for item in Data'Range (3) loop
   --              Sum := 0.0;
   --              for row in Data'Range loop
   --                 Sum := Sum + Data (row, Col, item);
   --              end loop;
   --              Result (col, item) := Sum;
   --           end loop;
   --        end loop;
   --
   --        return Result;
   --
   --     end Sum_Each_Column;

   --  ------------------------------------------------------------------------

end Support_21A;
