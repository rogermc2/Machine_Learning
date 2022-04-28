
--  with Ada.Assertions; use Ada.Assertions;

with Maths;

package body NL_Arrays_And_Matrices is

   function "*" (L : Float; R : Float_Matrix) return Float_Matrix is
      Result : Float_Matrix (R'First .. R'Last, R'First (2) .. R'Last (2));
   begin
      for row in R'Range loop
         for col in R'Range (2) loop
            Result (row, col) := L * R (row, col);
         end loop;
      end loop;

      return Result;

   end "*";

   --  ------------------------------------------------------------------------

   function "*" (L, R : Float_Matrix) return Float_Matrix is
      Result : Float_Matrix (L'First .. L'Last, L'First (2) .. L'Last (2));
   begin
      for row in L'Range loop
         for col in L'Range (2) loop
            Result (row, col) := L (row, col) * R (row, col);
         end loop;
      end loop;

      return Result;

   end "*";

   --  ------------------------------------------------------------------------

   function "**" (L : Float_Array; P : Integer) return Float_Array is
      Result : Float_Array := L;
   begin
      for row in Result'Range loop
         Result (row) := Result (row) ** P;
      end loop;

      return Result;

   end "**";

   --  ------------------------------------------------------------------------

   function "**" (L : Float_Matrix; P : Integer) return Float_Matrix is
      Result : Float_Matrix (L'First .. L'Last, L'First (2) .. L'Last (2));
   begin
      for row in L'Range loop
         for col in L'Range (2) loop
            Result (row, col) := L (row, col) ** P;
         end loop;
      end loop;

      return Result;

   end "**";

   --  ------------------------------------------------------------------------

   function "/" (L : Float_Matrix; R : Float) return Float_Matrix is
      Result : Float_Matrix (L'First .. L'Last, L'First (2) .. L'Last (2));
   begin
      for row in L'Range loop
         for col in L'Range (2) loop
            Result (row, col) := L (row, col) / R;
         end loop;
      end loop;

      return Result;

   end "/";

   --  ------------------------------------------------------------------------

   function "/" (L, R : Float_Matrix) return Float_Matrix is
      Result : Float_Matrix (L'First .. L'Last, L'First (2) .. L'Last (2));
   begin
      for row in L'Range loop
         for col in L'Range (2) loop
            Result (row, col) := L (row, col) / R (row, col);
         end loop;
      end loop;

      return Result;

   end "/";

   --  ------------------------------------------------------------------------

   function "+" (L, R : Float_Array) return Float_Array is
      Result : Float_Array (L'First .. L'Last);
   begin
      for row in L'Range loop
         Result (row) := L (row) + R (row);
      end loop;

      return Result;

   end "+";

   --  ------------------------------------------------------------------------

   function "+" (L, R : Float_Matrix) return Float_Matrix is
      Result : Float_Matrix (L'First .. L'Last, L'First (2) .. L'Last (2));
   begin
      for row in L'Range loop
         for col in L'Range (2) loop
            Result (row, col) := L (row, col) + R (row, col);
         end loop;
      end loop;

      return Result;

   end "+";

   --  ------------------------------------------------------------------------

   function "+" (L : Float_Matrix; R : Float_Array) return Float_Matrix is
      Result : Float_Matrix  := L;
   begin
      for row in L'Range loop
         for col in L'Range (2) loop
            Result (row, col) := Result (row, col) + R (col);
         end loop;
      end loop;

      return Result;

   end "+";

   --  ------------------------------------------------------------------------

   function "-" (M : Float_Matrix) return Float_Matrix is
      Result : Float_Matrix (M'Range, M'Range (2));
   begin
      for row in M'Range loop
         for col in M'Range (2) loop
            Result (row, col) := - M (row, col);
         end loop;
      end loop;

      return Result;

   end "-";

   --  ----------------------------------------------------------------------------

   function "-" (L, R : Float_Matrix) return Float_Matrix is
      Result : Float_Matrix (L'First .. L'Last, L'First (2) .. L'Last (2));
   begin
      for row in L'Range loop
         for col in L'Range (2) loop
            Result (row, col) := L (row, col) - R (row, col);
         end loop;
      end loop;

      return Result;

   end "-";

   --  ----------------------------------------------------------------------------

   function "-" (L, R : Integer_Matrix) return Integer_Matrix is
      Result : Integer_Matrix (L'First .. L'Last, L'First (2) .. L'Last (2));
   begin
      for row in L'Range loop
         for col in L'Range (2) loop
            Result (row, col) := L (row, col) - R (row, col);
         end loop;
      end loop;

      return Result;

   end "-";

   --  ----------------------------------------------------------------------------

   function "-" (L : Float; R : Float_Matrix) return Float_Matrix is
      Result : Float_Matrix (R'Range, R'Range (2));
   begin
      for row in R'Range loop
         for col in R'Range (2) loop
            Result (row, col) := L - R (row, col);
         end loop;
      end loop;

      return Result;

   end "-";

   --  ----------------------------------------------------------------------------
   --  Diff_Max returns the maximum value of each row
   function Diff_Max (Data : Float_Matrix) return Float_Matrix is
        aRow      : Float_Array (Data'Range (2));
        Max_Value : Float;
        Max_Vals  : Float_Array (Data'Range);
        Diff      : Float_Matrix := Data;
   begin
        for row in Data'Range loop
            aRow := Get_Row (Data, row);
            Max_Value := Float'Safe_First;
            for col in aRow'Range loop
                if aRow (col) > Max_Value then
                    Max_Value := aRow (col);
                end if;
            end loop;
            Max_Vals (row) := Max_Value;
        end loop;

        for row in Data'Range loop
            for col in aRow'Range loop
                Diff (row, col) := Diff (row, col) - Max_Vals (row);
            end loop;
        end loop;

      return Diff;

   end Diff_Max;

   --  ------------------------------------------------------------------------
   --  For A = [a(1,1) ... a(m,n)] and B = [b(1,1) ... b(n,p)]
   --  A.B = [c(i,j)] where
   --  c(i,j) = a(i,1)*b(1,j) + ... a(i,n)*b(n,j)
   function Dot (L, R : Float_Matrix) return Float_Matrix is
      --          Routine_Name : constant String := "NL_Arrays_And_Matrices.Dot ";
      Num_Rows : constant Positive := L'Length;
      Num_Cols : constant Positive := R'Length (2);
      Product  : Float_Matrix  (1 .. Num_Rows, 1 .. Num_Cols);
   begin
      for row in Product'Range loop
         for col in Product'Range (2) loop
            Product (row, col) := 0.0;
            for index in R'Range loop
               Product (row, col) := Product (row, col) +
                 L (row, index) * R (index, col);
            end loop;
         end loop;
      end loop;

      return Product;

   end Dot;

   --  ------------------------------------------------------------------

   function Dot (L : Float_Matrix; R : Float_Array) return Float_Array is
      Sum    : Float;
      Result : Float_Array (L'Range);
   begin
      for row in L'Range loop
         Sum := 0.0;
         for col in L'Range (2) loop
            Sum := Sum + L (row, col) * R (col);
         end loop;
         Result (row) := Sum;
      end loop;

      return Result;

   end Dot;

   --  ----------------------------------------------------------------------------

   function Exp (M : Float_Matrix) return Float_Matrix is
      use Maths.Float_Math_Functions;
      Result : Float_Matrix (M'Range, M'Range (2));
   begin
      for row in M'Range loop
         for col in M'Range (2) loop
            Result (row, col) := Exp (M (row, col));
         end loop;
      end loop;

      return Result;

   end Exp;

   --  ------------------------------------------------------------------------

   function To_Float_Array (List : NL_Types.Float_List) return Float_Array is
      Result : Float_Array (1 .. Positive (List.Length));
   begin
      for row in List.First_Index .. List.Last_Index loop
         Result (row) := List (row);
      end loop;

      return Result;

   end To_Float_Array;

   --  ------------------------------------------------------------------------

   function Flatten (M : Float_Matrix) return Float_Array is
      Flat : Float_Array (1 .. M'Length * M'Length (2));
   begin
      for row in M'Range loop
         for col in M'Range (2) loop
            Flat ((row - 1) * M'Length (2) + col) := M (row, col);
         end loop;
      end loop;

      return Flat;

   end Flatten;

   --  ------------------------------------------------------------------------

   function Flatten (M : Integer_Matrix) return Integer_Array is
      Flat : Integer_Array (1 .. M'Length * M'Length (2));
   begin
      for row in M'Range loop
         for col in M'Range (2) loop
            Flat ((row - 1) * M'Length (2) + col) := M (row, col);
         end loop;
      end loop;

      return Flat;

   end Flatten;

   --  ------------------------------------------------------------------------

   function Get_Row (M : Float_Matrix; R : Integer) return Float_Array is
      theRow : Float_Array (M'Range (2));
   begin
      for col in theRow'Range loop
         theRow (col) := M (R, col);
      end loop;

      return theRow;

   end Get_Row;

   --  ------------------------------------------------------------------------

   function "not" (M : Boolean_Matrix) return Boolean_Matrix is
      Result : Boolean_Matrix := M;
   begin
      for row in M'Range loop
         for col in M'Range (2) loop
            Result (row, col) := not Result (row, col);
         end loop;
      end loop;

      return Result;

   end "not";

   --  ------------------------------------------------------------------------

   function To_Boolean_Array (List : NL_Types.Boolean_List)
                              return Boolean_Array is
   begin
      if not List.Is_Empty then
         declare
            Result : Boolean_Array (List.First_Index .. List.Last_Index);
         begin
            for row in Result'Range loop
               Result (row) := List (row);
            end loop;
            return Result;
         end;
      else
         declare
            Result : Boolean_Array (1 .. 0);
         begin
            return Result;
         end;
      end if;

   end To_Boolean_Array;

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

   function To_Float_Matrix (List : NL_Types.Float_List_2D)
                              return Float_Matrix is
   begin
      if not List.Is_Empty then
         declare
            List_Row : NL_Types.Float_List;
            Result   : Float_Matrix (List.First_Index .. List.Last_Index,
                                     List (1).First_Index ..
                                       List (1).Last_Index);
         begin
            for row in Result'Range loop
               List_Row := List (row);
               for col in Result'Range (2) loop
                  Result (row, col) := List_Row (col);
               end loop;
            end loop;
            return Result;
         end;
      else
         declare
            Result : Float_Matrix (1 .. 0, 1 .. 0);
         begin
            return Result;
         end;
      end if;

   end To_Float_Matrix;

   --  ------------------------------------------------------------------------

   function To_Integer_Array (List : NL_Types.Integer_List)
                               return Integer_Array is
   begin
      if not List.Is_Empty then
         declare
            Result : Integer_Array (List.First_Index .. List.Last_Index);
         begin
            for row in Result'Range loop
               Result (row) := List (row);
            end loop;
            return Result;
         end;
      else
         declare
            Result : Integer_Array (1 .. 0);
         begin
            return Result;
         end;
      end if;

   end To_Integer_Array;

   --  ------------------------------------------------------------------------

   function To_Natural_Array (List : NL_Types.Natural_List)
                               return Natural_Array is
   begin
      if not List.Is_Empty then
         declare
            Result : Natural_Array (List.First_Index .. List.Last_Index);
         begin
            for row in Result'Range loop
               Result (row) := List (row);
            end loop;
            return Result;
         end;
      else
         declare
            Result : Natural_Array (1 .. 0);
         begin
            return Result;
         end;
      end if;
   end To_Natural_Array;

   --  ------------------------------------------------------------------------

   function Transpose (Values : Boolean_Matrix) return Boolean_Matrix is
      Result : Boolean_Matrix (1 .. Values'Length (2), 1 .. Values'Length);
   begin
      for row in Values'First .. Values'Last loop
         for col in Values'First (2) .. Values'Last (2) loop
            Result (col, row) := Values (row, col);
         end loop;
      end loop;

      return Result;

   end Transpose;

   --  -------------------------------------------------------------------------

   function Transpose (Values : Float_Matrix) return  Float_Matrix is
      Result : Float_Matrix (1 .. Values'Length (2), 1 .. Values'Length);
   begin
      for row in Values'First .. Values'Last loop
         for col in Values'First (2) .. Values'Last (2) loop
            Result (col, row) := Values (row, col);
         end loop;
      end loop;

      return Result;

   end Transpose;

   --  -------------------------------------------------------------------------

   function Transpose (Values : Integer_Matrix) return  Integer_Matrix is
      Result   : Integer_Matrix (1 .. Values'Length (2), 1 .. Values'Length);
   begin
      for row in Values'Range loop
         for col in Values'Range (2) loop
            Result (col, row) := Values (row, col);
         end loop;
      end loop;

      return Result;

   end Transpose;

   --  -------------------------------------------------------------------------

end NL_Arrays_And_Matrices;
