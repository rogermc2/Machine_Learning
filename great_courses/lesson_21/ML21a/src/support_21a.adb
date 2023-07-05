
with Ada.Assertions; use Ada.Assertions;
with Ada.Exceptions; use Ada.Exceptions;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded;

with Basic_Printing; use Basic_Printing;

package body Support_21A is

   type Actions_Array is array (Integer range <>) of Actions_Range;

   function Compute_Map_Matrix (Grid_Map : Integer_Matrix; Num_Cats : Positive)
                                return Boolean_Tensor;
   function Compute_Q (Mat_Trans : Boolean_Tensor; v : Real_Float_Matrix;
                       Num_Acts : Integer) return Real_Float_Matrix;
   function Compute_Transition_Matrix
     (Num_Rows, Num_Cols, Num_Actions : Positive; Actions : Actions_Matrix;
      Mat_Map : Boolean_Tensor) return Boolean_Tensor;
   function Dot_Trans_V (Trans : Boolean_Tensor; Trans_Row : Positive;
                         V : Real_Float_Matrix) return Real_Float_Matrix;
   function Get_Action (Matrix : Actions_Matrix; Row : Integer)
                        return Actions_Array;
   function Pi_Max (Policy : Real_Float_Matrix; Row : Positive) return Float;
   function Product (L : Boolean_Tensor; R : Integer_Matrix) return Integer_Tensor;
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
                      return Boolean_Tensor is
      use Real_Float_Arrays;
      Routine_Name : constant String := "Support_21A.Binarize ";
      Rewards           : constant Integer_Array (Grid_Map'Range) :=
        (0, -1, -1, -1, 10);
      Num_Actions       : constant Positive := 5;
      Rows_x_Cols       : constant Positive := Num_Rows * Num_Cols;
      --  Acts defines how each action changes the row and column
      Actions           : constant Actions_Matrix (1 .. Num_Actions, 1 ..2) :=
        ((-1,0), (0,1), (1,0), (0,-1), (0,0));
      --  Mat_Map is a binarised version of Grid_Map in which the value of
      --  Mat_Map is 1 (otherwise 0) if the Grid_Map row and column
      --  equals the index of the third dimension of the cell.
      Mat_Map           : constant Boolean_Tensor :=
        Compute_Map_Matrix (Grid_Map, Num_Cats);
      --  Mat_Transition indicates whether or not a given action will cause
      --  a transition between a given pair of locations.
      Mat_Transition : constant Boolean_Tensor :=
        Compute_Transition_Matrix (Num_Rows, Num_Cols, Num_Actions, Actions,
                                   Mat_Map);
      Beta              : constant Float := 10.0;
      Gamma             : constant Float := 0.9;
      Q                 : Real_Float_Matrix (1 .. Rows_x_Cols,
                                             1 .. Num_Actions);
      rk                : Integer_Matrix (1 .. Num_Rows, 1 .. 1);
      rfk               : Integer_Tensor (1 .. Num_Rows, 1 .. Num_Cols, 1 .. 1);
      rffk              : Real_Float_Matrix (1 .. Rows_x_Cols, 1 .. 1);
      v                 : Real_Float_Matrix (1 .. Rows_x_Cols, 1 .. 1);
      Policy            : Real_Float_Matrix (Q'Range, Q'Range (2));
      Pi_Q              : Real_Float_Matrix (Q'Range, Q'Range (2));
      Pi_Q_Sum          : Real_Float_Vector (Q'Range (2));
   begin
      --  Rewards   (0, -1, -1, -1, 10);
      for row in Rewards'Range loop
         rk (row, 1) := Rewards (row);
      end loop;
      Print_Integer_Matrix (Routine_Name & "rk", rk);

      --  rfk maps each location to its reward value
      rfk := Product (Mat_Map, rk);

      for row in rfk'Range loop
         for col in rfk'Range (2) loop
            rffk ((row - 1) * rfk'Length (2) + col, 1) :=
              Float (rfk (row, col, 1));
         end loop;
      end loop;
      v := rffk;

      for count in 1 .. 50 loop
         --        for count in 1 .. 5 loop
         Q := Compute_Q (Mat_Transition, v, Num_Actions);
         Policy := Python.Call (Classifier, "softmax", Beta * Q);
         Pi_Q := H_Product (Q, Policy);
         Pi_Q_Sum := Sum_Each_Column (Pi_Q);
         v := rffk + gamma * Pi_Q_Sum;
      end loop;
      --        Print_Float_Matrix (Routine_Name & "Pi", Pi);

      Plot_Policy (Policy, Actions, Num_Rows, Num_Cols);

      return Mat_Transition;

   end Binarize;

   --  -------------------------------------------------------------------------
   --  Clip keeps the current grid location within the grid boundary
   function Clip (Value, Min, Max : Integer) return Integer is
      Result : Integer := Value;
   begin
      if Result < Min then
         Result := Min;
      elsif Result > Max then
         Result := Max;
      end if;

      return Result;

   end Clip;

   --  -------------------------------------------------------------------------
   --  Compute_Map_Matrix generates a binarised version of Grid_Map in which the
   --  value of each element is 1 (otherwise 0) if the Grid_Map row and column
   --  equals the index of the third dimension of the cell.
   function Compute_Map_Matrix (Grid_Map : Integer_Matrix; Num_Cats : Positive)
                                return Boolean_Tensor is
      Mat_Map   : Boolean_Tensor (Grid_Map'Range, Grid_Map'Range (2),
                                  1 .. Num_Cats);
   begin
      for row in Mat_Map'Range loop
         for col in Mat_Map'Range (2) loop
            for cat in Mat_Map'Range (3) loop
               Mat_Map (row, col, cat) := Grid_Map (row, col) = cat;
            end loop;
         end loop;
      end loop;

      return Mat_Map;

   end Compute_Map_Matrix;

   --  -------------------------------------------------------------------------

   --  Compute_Transition_Matrix generates a transition matrix that indicates
   --  whether or not a given action will cause a transition between a given
   --  pair of locations.
   function Compute_Transition_Matrix
     (Num_Rows, Num_Cols, Num_Actions : Positive; Actions : Actions_Matrix;
      Mat_Map : Boolean_Tensor) return Boolean_Tensor is
      Rows_x_Cols       : constant Positive := Num_Rows * Num_Cols;
      Current_Action    : Actions_Array (1 .. 2);
      Row_Next          : Positive;
      Col_Next          : Positive;
      Trans_Tensor      : Boolean_Tensor (1 .. Num_Actions, 1 .. Rows_x_Cols,
                                          1 .. Rows_x_Cols);
   begin
      for act in Actions'Range loop
         --  Current_Action specifies changes to the row and column
         Current_Action := Get_Action (Actions, act);
         for row in Mat_Map'Range  loop
            for col in Mat_Map'Range (2) loop
               --  Clip keeps this grid location inside the grid boundary
               --  Row_Next and Col_Next are the changed row and column
               Row_Next := Clip (row + Current_Action (1), 1, Num_Rows);
               Col_Next := Clip (col + Current_Action (2), 1, Num_Cols);
               for row_2 in Mat_Map'Range  loop
                  for col_2 in Mat_Map'Range (2) loop
                     Trans_Tensor (act, (row - 1) * Num_Cols + col,
                                   (row_2 - 1) * Num_Cols + col_2) :=
                       (row_2 = Row_Next) and (col_2 = Col_Next);
                  end loop;
               end loop;
            end loop;
         end loop;
      end loop;

      return Trans_Tensor;

   end Compute_Transition_Matrix;

   --  -------------------------------------------------------------------------

   --  V reflects how good it is to be in each location looking ahead an
   --  additional step.
   function Compute_Q (Mat_Trans : Boolean_Tensor; v : Real_Float_Matrix;
                       Num_Acts  : Integer) return Real_Float_Matrix is
      Routine_Name : constant String := "Support_21A.Compute_Q ";
      Q            : Real_Float_Matrix (Mat_Trans'Range (2), Mat_Trans'Range) :=
        (others =>  (others => 0.0));
   begin
      --  Mat_Trans 5, 50, 50; Num_Acts, Num_Rows_x_Num_Cols, Num_Rows_x_Num_Cols
      --  V 50, 1
      --        Put_Line (Routine_Name & "Mat_Trans dim:" &
      --                    Integer'Image (Mat_Trans'Length) &
      --                    Integer'Image (Mat_Trans'Length (2)) &
      --                    Integer'Image (Mat_Trans'Length (3)));
      --        Print_Float_Matrix (Routine_Name & "v", v);
      for act_index in 1 .. Num_Acts loop
         declare
            Q_Act : constant Real_Float_Matrix :=
              Dot_Trans_V (Mat_Trans, act_index, v);
         begin
            --              Print_Float_Matrix (Routine_Name & "Q_Act", Q_Act);
            for row in Q_Act'Range loop
               for col in Q_Act'Range (2) loop
                  Assert (Q_Act (row, col)'Valid, Routine_Name &
                            "invalid Q_Act (row, col): " & Integer'Image (row) &
                            Integer'Image (col) & ": " &
                            Float'Image  (Q_Act (row, col)));
                  Q (row, col) := Q_Act (row, col);
               end loop;
            end loop;
         end;
      end loop;

      --  Q 50, 5
      return Q;

   end Compute_Q;

   --  -------------------------------------------------------------------------

   function Dot_Trans_V (Trans : Boolean_Tensor; Trans_Row : Positive;
                         V     : Real_Float_Matrix) return Real_Float_Matrix is
      use Real_Float_Arrays;
      --        Routine_Name : constant String := "Support_21A.Dot_Trans_V ";
      Act_Mat    : Real_Float_Matrix (Trans'Range (2), Trans'Range (3)) :=
        (others => (others => 0.0));
   begin
      --  Trans 5, 50, 50; Num_Acts, Num_Rows_x_Num_Cols, Num_Rows_x_Num_Cols
      --  V 50, 1
      --  Act_Mat 50, 50
      for row in Act_Mat'Range loop
         for col in Act_Mat'Range (2) loop
            if Trans (Trans_Row, row, col) then
               Act_Mat (row, col) := 1.0;
            end if;
         end loop;
      end loop;

      --  Act_Mat * V   50, 1
      return Act_Mat * V;

   end Dot_Trans_V;

   --  -------------------------------------------------------------------------

   procedure Find_Policy (Policy_Grid : in out Integer_Matrix;
                          Policy : Real_Float_Matrix; Actions : Actions_Matrix;
                          Row_In, Col_In : Positive) is
      Routine_Name : constant String := "Support_21A.Find_Policy ";
      Num_Cols : constant Positive := Policy_Grid'Length (2);
      Num_Rows : constant Positive := Policy_Grid'Length;
      Row      : Positive := Row_In;
      Col      : Positive := Col_In;
      Pi_Row   : Positive;
      Max_Prob : Float;
      A        : Natural := 6;
   begin
      Put_Line (Routine_Name & "Num_Rows, Num_Cols:" &
                  Integer'Image (Num_Rows) & Integer'Image (Num_Cols));
      Put_Line (Routine_Name & "Row, Col:" &
                  Integer'Image (Row) & Integer'Image (Col));
      Put_Line (Routine_Name & "Policy_Grid (Row, Col): " &
                  Integer'Image (Policy_Grid (Row, Col)));
      Assert (Policy (Row, 1)'Valid, Routine_Name & "invalid Pi: " &
                 Float'Image  (Policy (Row, 1)));
      --        Print_Float_Matrix (Routine_Name & "Pi", Pi);
      while Policy_Grid (Row, Col) = 6 loop
         Pi_Row := (Row - 1) * Num_Cols + 1;
         Max_Prob := Pi_Max (Policy, Pi_Row + Col - 1);
         Put_Line (Routine_Name & "Max_Prob: " & Float'Image (Max_Prob));
         for ana in 1 .. 5 loop
            if Policy (Pi_Row, ana) = Max_Prob then
               A := ana;
            end if;
         end loop;
         Put_Line (Routine_Name & "A" & Integer'Image (A));
         Put_Line (Routine_Name & "Col" & Integer'Image (Col));
         Put_Line (Routine_Name & "Actions (A-1, 2): " &
                     Integer'Image (Actions (A-1, 2)));

         Policy_Grid (Row, Col) := A;
         Row := Clip (Row + Actions (A, 1), 1, Num_Rows);
         Col := Clip (Col + Actions (A, 2), 1, Num_Cols);
         Find_Policy (Policy_Grid, Policy, Actions, Row, Col);
      end loop;

   exception
      when Error: Constraint_Error => Put_Line (Routine_Name &
                                                  "Constraint_Error");
         Put_Line (Exception_Information(Error));
      when Error: others => Put_Line (Routine_Name & "exception");
         Put_Line (Exception_Information(Error));
   end Find_Policy;

   --  -------------------------------------------------------------------------

   function Get_Action (Matrix : Actions_Matrix; Row : Integer)
                        return Actions_Array is
      Result : Actions_Array (Matrix'Range (2));
   begin
      for col in Matrix'Range (2) loop
         Result (col) := Matrix  (Row, col);
      end loop;

      return Result;

   end Get_Action;

   --  ------------------------------------------------------------------------

   function Pi_Max (Policy : Real_Float_Matrix; Row : Positive) return Float is
      Routine_Name : constant String := "Support_21A.Pi_Max ";
      Result       : Float := Policy (Row, 1);
   begin
      Put_Line (Routine_Name & "Result" & Float'Image (Result));
      for col in Policy'Range (2) loop
         if Policy (Row, col) > Result then
            Result := Policy (Row, col);
         end if;
      end loop;

      return Result;

   end Pi_Max;

   --  -------------------------------------------------------------------------

   procedure Plot_Policy (Policy : Real_Float_Matrix; Actions : Actions_Matrix;
                          Num_Rows, Num_Cols : Positive) is
      use Ada.Strings.Unbounded;
      --        Routine_Name : constant String := "Support_21A.Plot_Policy ";
      Signs       : constant String (1 .. 7) := "^>v<x? ";
      Policy_Grid : Integer_Matrix (1 .. Num_Rows, 1 .. Num_Cols) := (others => (others => 6));
      Line        : Unbounded_String;
   begin
      Find_Policy (Policy_Grid, Policy, Actions, 1, 1);

      for row in 1 .. Num_Rows loop
         Line := To_Unbounded_String ("");
         for col in 1 .. Num_Cols loop
            Line := Line & Signs (Policy_Grid (row, col) + 1);
         end loop;
         Put_Line (To_String (Line));
      end loop;

   end Plot_Policy;

   --  -------------------------------------------------------------------------

   --  for matrix L of dimensions (m,n,p) and R of dimensions (p,s)
   --  C(i, j, k) = sum[r=1 to p] L(i, j, r) * R(r, k)
   function Product (L : Boolean_Tensor; R : Integer_Matrix) return Integer_Tensor is
      Routine_Name : constant String := "Support_21A.Product ";
      L_Int        : Natural;
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
                  if L(li, lj, rr) then
                     L_Int := 1;
                  else
                     L_Int := 0;
                  end if;
                  --  Result(i, j, k) = sum (L(i, j, r) * R(r, k))
                  Sum := Sum + L_Int * R (rr, rk);
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
