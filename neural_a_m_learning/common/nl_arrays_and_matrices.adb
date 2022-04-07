
package body NL_Arrays_And_Matrices is

   function "*" (L, R : Float_Matrix) return Float_Matrix is
      Result : Float_Matrix (L'First .. L'Last, L'First (2) .. L'Last (2));
   begin
      for row in L'First .. L'Last loop
         for col in L'First (2) .. L'Last (2) loop
            Result (row, col) := L (row, col) * R (row, col);
         end loop;
      end loop;

      return Result;

   end "*";

   --  ------------------------------------------------------------------------

   function "**" (L : Float_Matrix; P : Integer) return Float_Matrix is
      Result : Float_Matrix (L'First .. L'Last, L'First (2) .. L'Last (2));
   begin
      for row in L'First .. L'Last loop
         for col in L'First (2) .. L'Last (2) loop
            Result (row, col) := L (row, col) ** P;
         end loop;
      end loop;

      return Result;

   end "**";

   --  ------------------------------------------------------------------------

   function "/" (L : Float_Matrix; R : Float) return Float_Matrix is
      Result : Float_Matrix (L'First .. L'Last, L'First (2) .. L'Last (2));
   begin
      for row in L'First .. L'Last loop
         for col in L'First (2) .. L'Last (2) loop
            Result (row, col) := L (row, col) / R;
         end loop;
      end loop;

      return Result;

   end "/";

   --  ------------------------------------------------------------------------

   function "/" (L, R : Float_Matrix) return Float_Matrix is
      Result : Float_Matrix (L'First .. L'Last, L'First (2) .. L'Last (2));
   begin
      for row in L'First .. L'Last loop
         for col in L'First (2) .. L'Last (2) loop
            Result (row, col) := L (row, col) / R (row, col);
         end loop;
      end loop;

      return Result;

   end "/";

   --  ------------------------------------------------------------------------

   function "-" (L, R : Float_Matrix) return Float_Matrix is
      Result : Float_Matrix (L'First .. L'Last, L'First (2) .. L'Last (2));
   begin
      for row in L'First .. L'Last loop
         for col in L'First (2) .. L'Last (2) loop
            Result (row, col) := L (row, col) - R (row, col);
         end loop;
      end loop;

      return Result;

   end "-";

   --  ------------------------------------------------------------------------

   function Dot (L, R : Float_Matrix) return Float_Matrix is
      Num_Rows : constant Positive := L'Length;
      Num_Cols : constant Positive := L'Length (2);
      RT      : constant Float_Matrix := Transpose (R);
      L_Row   : Float_Array (1 .. Num_Cols);
      LR      : Float;
      Product : Float_Matrix  (1 .. Num_Rows, 1 .. Num_Cols);
   begin
      for row in Product'First .. Product'Last loop
         for col in Product'First (2) .. Product'Last (2) loop
            L_Row (col) := L (row, col);
            LR := 0.0;
            for lr_index in L_Row'First .. L_Row'Last loop
               LR := LR + L_Row (lr_index) * RT (row, lr_index);
            end loop;
            Product (row, col) := LR;
         end loop;
      end loop;

      return Product;

   end Dot;

   --  ----------------------------------------------------------------------------

   function To_Float_Array (List : Float_List) return Float_Array is
      Result : Float_Array (1 .. Positive (List.Length));
   begin
      for row in List.First_Index .. List.Last_Index loop
            Result (row) := List (row);
      end loop;

      return Result;

   end To_Float_Array;

   --  ------------------------------------------------------------------------

   function To_Float_Matrix (IM : Integer_Matrix) return Float_Matrix is
      Result : Float_Matrix (1 .. IM'Length, 1 .. IM'Length (2));
   begin
      for row in IM'First .. IM'Last loop
         for col in IM'First (2) .. IM'Last (2) loop
            Result (row, col) := Float (IM (row, col));
         end loop;
      end loop;

      return Result;

   end To_Float_Matrix;

   --  ------------------------------------------------------------------------

   function To_Float_Matrix (List : Float_List_2D) return Float_Matrix is
      List_Row : Float_List;
      Result   : Float_Matrix (1 .. Positive (List.Length),
                               1 .. Positive (List (1).Length));
   begin
      for row in List.First_Index .. List.Last_Index loop
         List_Row := List (row);
         for col in List_Row.First_Index .. List_Row.Last_Index loop
            Result (row, col) := List_Row (col);
         end loop;
      end loop;

      return Result;

   end To_Float_Matrix;

   --  ------------------------------------------------------------------------

   function To_Integer_Array (List : Integer_List) return Integer_Array is
      Result : Integer_Array (1 .. Positive (List.Length));
   begin
      for row in List.First_Index .. List.Last_Index loop
            Result (row) := List (row);
      end loop;

      return Result;

   end To_Integer_Array;

   --  ------------------------------------------------------------------------

   function Transpose (Values : Float_Matrix) return  Float_Matrix is
      Result   : Float_Matrix (1 .. Values'Length (2), 1 .. Values'Length);
   begin
      for row in Values'First .. Values'Last loop
         for col in Values'First (2) .. Values'Last (2) loop
            Result (col, row) := Values (row, col);
         end loop;
      end loop;

      return Result;

   end Transpose;

   --  -------------------------------------------------------------------------

end NL_Arrays_And_Matrices;
