
with Ada.Assertions; use Ada.Assertions;
with Ada.Text_IO; use Ada.Text_IO;

with Maths;

--  with Basic_Printing; use Basic_Printing;
package body ML_Arrays_And_Matrices is

   --  ------------------------------------------------------------------------

   function ">=" (L : Real_Float_Vector; R : Float) return Boolean_Array is
      Result : Boolean_Array (L'Range);
   begin
      for row in L'Range loop
         Result (row) := L (row) >= R;
      end loop;

      return Result;

   end ">=";

   --  ------------------------------------------------------------------------

   function ">=" (L, R : Real_Float_Vector) return Boolean_Array is
      Result : Boolean_Array (L'Range);
   begin
      for row in L'Range loop
         Result (row) := L (row) >= R (row);
      end loop;

      return Result;

   end ">=";

   --  ------------------------------------------------------------------------

   function ">=" (L : Real_Float_Vector; R : Float) return Real_Float_Vector is
      Result : Real_Float_Vector (L'Range) := (others => 1.0);
   begin
      for row in L'Range loop
         if L (row) < R then
            Result (row) := -1.0;
         end if;
      end loop;

      return Result;

   end ">=";

   --  ------------------------------------------------------------------------

   function ">=" (L : Real_Float_Vector; R : Float) return Float_Array is
      Result : Float_Array (L'Range) := (others => 1.0);
   begin
      for row in L'Range loop
         if L (row) < R then
            Result (row) := -1.0;
         end if;
      end loop;

      return Result;

   end ">=";

   --  ------------------------------------------------------------------------

   function "<=" (L, R : Real_Float_Vector) return Boolean_Array is
      Result : Boolean_Array (L'Range);
   begin
      for row in L'Range loop
         Result (row) := L (row) <= R (row);
      end loop;

      return Result;

   end "<=";

   --  ------------------------------------------------------------------------

   function "<" (L, R : Integer_Array) return Boolean_Array is
      Result : Boolean_Array (L'Range);
   begin
      for row in L'Range loop
         Result (row) := L (row) < R (row);
      end loop;

      return Result;

   end "<";

   --  ------------------------------------------------------------------------

   function "<" (L, R : Real_Float_Vector) return Boolean_Array is
      Result : Boolean_Array (L'Range);
   begin
      for row in L'Range loop
         Result (row) := L (row) < R (row);
      end loop;

      return Result;

   end "<";

   --  ------------------------------------------------------------------------

   function "*" (L, R : Float_Array) return Float_Array is
      Result : Float_Array := L;
   begin
      for row in L'Range loop
         Result (row) := Result (row) * R (row);
      end loop;

      return Result;

   end "*";

   --  ------------------------------------------------------------------------

   function "*" (L : Float; R : Float_Array) return Float_Array is
      Result : Float_Array := R;
   begin
      for row in R'Range loop
         Result (row) := L * Result (row);
      end loop;

      return Result;

   end "*";

   --  ------------------------------------------------------------------------

   function "**" (L : Real_Float_Matrix; R : Natural) return Real_Float_Matrix is
      Result : Real_Float_Matrix (L'Range, L'Range (2));
   begin
      for row in L'Range loop
         for col in L'Range (2) loop
            Result (row, col) := L (row, col) ** R;
         end loop;
      end loop;

      return Result;

   end "**";

   --  ------------------------------------------------------------------------

   function "**" (L : Real_Float_Vector; R : Natural) return Real_Float_Vector is
      Result : Real_Float_Vector (L'Range);
   begin
      for index in L'Range loop
         Result (index) := L (index) ** R;
      end loop;

      return Result;

   end "**";

   --  ------------------------------------------------------------------------

   function "/" (L, R : Binary_Array) return Real_Float_Vector is
      Result : Real_Float_Vector (R'Range);
   begin
      for index in R'Range loop
         Result (index) := Float (L (index)) / Float (R (index));
      end loop;

      return Result;

   end "/";

   --  ------------------------------------------------------------------------

   function "/" (L : Float; R : Real_Float_Matrix) return Real_Float_Matrix is
      Result : Real_Float_Matrix (R'Range, R'Range (2));
   begin
      for row in R'Range loop
         for col in R'Range (2) loop
            Result (row, col) := L / R (row, col);
         end loop;
      end loop;

      return Result;

   end "/";

   --  ------------------------------------------------------------------------

   function "/" (L : Float; R : Real_Float_Vector) return Real_Float_Vector is
      Result : Real_Float_Vector (R'Range);
   begin
      for index in R'Range loop
         Result (index) := L / R (index);
      end loop;

      return Result;

   end "/";

   --  ------------------------------------------------------------------------

   function "/" (L, R : Real_Float_Matrix) return Real_Float_Matrix is
      Result : Real_Float_Matrix := L;
   begin
      for row in L'Range loop
         for col in L'Range (2) loop
            Result (row, col) := Result (row, col) / R (row, col);
         end loop;
      end loop;

      return Result;

   end "/";

   --  ------------------------------------------------------------------------

   function "/" (L : Real_Float_Matrix; R : Real_Float_Vector)
                 return Real_Float_Matrix is
      R_Val  : Float;
      Result : Real_Float_Matrix := L;
   begin
      for row in L'Range loop
         R_Val := 1.0 / R (row);
         for col in L'Range (2) loop
            Result (row, col) := R_Val * Result (row, col);
         end loop;
      end loop;

      return Result;

   end "/";

   --  ------------------------------------------------------------------------

   function "/" (L, R : Real_Float_Vector) return Real_Float_Vector is
      Result : Real_Float_Vector := L;
   begin
      for row in L'Range loop
         Result (row) := Result (row) / R (row);
      end loop;

      return Result;

   end "/";

   --  ------------------------------------------------------------------------

   function "+" (L, R : Integer_Matrix) return Integer_Matrix is
      Result : Integer_Matrix (L'First .. L'Last, L'First (2) .. L'Last (2));
   begin
      for row in L'Range loop
         for col in L'Range (2) loop
            Result (row, col) := L (row, col) + R (row, col);
         end loop;
      end loop;

      return Result;

   end "+";

   --  ----------------------------------------------------------------------------

   function "+" (L : Real_Float_Matrix; R : Real_Float_Vector)
                 return Real_Float_Matrix is
      Result : Real_Float_Matrix := L;
   begin
      for row in L'Range loop
         for col in L'Range (2) loop
            Result (row, col) := Result (row, col) + R (col);
         end loop;
      end loop;

      return Result;

   end "+";

   --  ------------------------------------------------------------------------

   function "+" (L : Float; R : Real_Float_Matrix) return Real_Float_Matrix is
      Result : Real_Float_Matrix := R;
   begin
      for row in R'Range loop
         for col in R'Range (2) loop
            Result (row, col) := Result (row, col) + L;
         end loop;
      end loop;

      return Result;

   end "+";

   --  ------------------------------------------------------------------------

   function "+" (L : Float; R : Real_Float_Vector) return Real_Float_Vector is
      Result : Real_Float_Vector := R;
   begin
      for row in R'Range loop
         Result (row) := Result (row) + L;
      end loop;

      return Result;

   end "+";

   --  ------------------------------------------------------------------------

   function "-" (L, R : Binary_Array) return Binary_Array is
      Result : Binary_Array (L'Range);
   begin
      for index in L'Range loop
         Result (index) := L (index) - R (index);
      end loop;

      return Result;

   end "-";

   --  ----------------------------------------------------------------------------

   function "-" (L, R : Binary_Matrix) return Binary_Matrix is
      Result : Binary_Matrix (L'Range, L'Range (2));
   begin
      for row in L'Range loop
         for col in L'Range (2) loop
            Result (row, col) := L (row, col) - R (row, col);
         end loop;
      end loop;

      return Result;

   end "-";

   --  ----------------------------------------------------------------------------

   function "-" (M : Integer_Matrix) return Integer_Matrix is
      Result : Integer_Matrix (M'Range, M'Range (2));
   begin
      for row in M'Range loop
         for col in M'Range (2) loop
            Result (row, col) := - M (row, col);
         end loop;
      end loop;

      return Result;

   end "-";

   --  ----------------------------------------------------------------------------

   function "-" (L, R : Integer_Matrix) return Integer_Matrix is
      Result : Integer_Matrix (L'Range, L'Range (2));
   begin
      for row in L'Range loop
         for col in L'Range (2) loop
            Result (row, col) := L (row, col) - R (row, col);
         end loop;
      end loop;

      return Result;

   end "-";

   --  ----------------------------------------------------------------------------

   function "-" (L : Real_Float_Matrix; R : Boolean_Matrix) return Real_Float_Matrix is
      Result : Real_Float_Matrix (L'Range, L'Range (2));
   begin
      for row in L'Range loop
         for col in L'Range (2) loop
            if R (row, col) then
               Result (row, col) := L (row, col) - 1.0;
            end if;
         end loop;
      end loop;

      return Result;

   end "-";

   --  ----------------------------------------------------------------------------

   function "-" (L : Real_Float_Matrix; R : Float) return Real_Float_Matrix is
      Result : Real_Float_Matrix (L'Range, L'Range (2));
   begin
      for row in L'Range loop
         for col in L'Range (2) loop
            Result (row, col) := L (row, col) - R;
         end loop;
      end loop;

      return Result;

   end "-";

   --  ----------------------------------------------------------------------------

   function "-" (L : Real_Float_Matrix; R : Real_Float_Vector)
                 return Real_Float_Matrix is
      Result : Real_Float_Matrix (L'Range, L'Range (2));
   begin
      for row in L'Range loop
         for col in L'Range (2) loop
            Result (row, col) := L (row, col) - R (row);
         end loop;
      end loop;

      return Result;

   end "-";

   --  ------------------------------------------------------------------------

   function "-" (L : Float; R : Real_Float_Vector) return Real_Float_Vector is
      Result : Real_Float_Vector (R'Range);
   begin
      for index in R'Range loop
         Result (index) := R (index) - L;
      end loop;

      return Result;

   end "-";

   --  ------------------------------------------------------------------------

   function "-" (L : Real_Float_Vector; R : Float) return Real_Float_Vector is
      Result : Real_Float_Vector := L;
   begin
      for index in L'Range loop
         Result (index) := L (index) - R;
      end loop;

      return Result;

   end "-";

   --  ------------------------------------------------------------------------

   function "&" (L, R : Integer_Matrix) return Integer_Matrix is
      Result : Integer_Matrix (1 .. L'Length + R'Length, L'First (2) .. L'Last (2));
   begin
      Assert (R'Length (2) = L'Length (2),
              "Attempt to concatenate matrices of different row sizes.");
      for row in L'Range loop
         for col in L'Range (2) loop
            Result (row, col) := L (row, col);
         end loop;
      end loop;

      for row in R'Range loop
         for col in R'Range (2) loop
            Result (L'Length + row, col) := R (row, col);
         end loop;
      end loop;

      return Result;

   end "&";

   --  ----------------------------------------------------------------------------

   procedure Check_Lengths (Routine_Name : String; L, R : Boolean_Matrix) is
   begin
      Assert (R'Length = L'Length and R'Length (2) = L'Length (2),
              Routine_Name &
                " Check_Lengths right size" & Integer'Image (R'Length) & " x" &
                Integer'Image (R'Length (2)) &
                " should be the same as left size" & Integer'Image (L'Length) &
                " x" & Integer'Image (L'Length (2)));
   end Check_Lengths;

   --  ----------------------------------------------------------------------------

   procedure Check_Lengths (Routine_Name : String; L, R : Integer_Matrix) is
   begin
      Assert (R'Length = L'Length and R'Length (2) = L'Length (2),
              Routine_Name &
                " right size" & Integer'Image (R'Length) & " x" &
                Integer'Image (R'Length (2)) &
                " should be the same as left size" & Integer'Image (L'Length) &
                " x" & Integer'Image (L'Length (2)));
   end Check_Lengths;

   --  ----------------------------------------------------------------------------

   procedure Check_Lengths
     (Routine_Name : String; L : Binary_Matrix; R : Integer_Matrix) is
   begin
      Assert (R'Length = L'Length and R'Length (2) = L'Length (2),
              Routine_Name &
                " right size" & Integer'Image (R'Length) & " x" &
                Integer'Image (R'Length (2)) &
                " should be the same as left size" & Integer'Image (L'Length) &
                " x" & Integer'Image (L'Length (2)));
   end Check_Lengths;

   --  ----------------------------------------------------------------------------

   procedure Check_Lengths (Routine_Name : String; L, R : Real_Float_Matrix) is
   begin
      Assert (R'Length = L'Length and R'Length (2) = L'Length (2),
              Routine_Name &
                " right size" & Integer'Image (R'Length) & " x" &
                Integer'Image (R'Length (2)) &
                " should be the same as left size" & Integer'Image (L'Length) &
                " x" & Integer'Image (L'Length (2)));
   end Check_Lengths;

   --  ----------------------------------------------------------------------------

   function Cumulative_Sum (A : Float_Array) return Float_Array is
      Result : Float_Array (A'Range);
   begin
      Result (A'First) := A (A'First);
      for index in A'First + 1 .. A'Last loop
         Result (index) := Result (index - 1) + A (index);
      end loop;

      return Result;

   end Cumulative_Sum;

   --  ----------------------------------------------------------------------------

   function Dot (L : Integer_Matrix; R : Real_Float_Vector)
                 return Real_Float_Vector is
      Sum    : Float;
      Result : Real_Float_Vector (L'Range);
   begin
      for row in L'Range loop
         Sum := 0.0;
         for col in R'Range loop
            Sum := Sum + Float (L (row, col)) * R (col);
            Assert (Sum'Valid, "Dot, Sum =" & Float'Image (Sum) & "row, col:"
                    & Integer'Image (row) & ", " & Integer'Image (col) &
                      ", L, R:" & Integer'Image (L (row, col)) & ", " &
                      Float'Image (R (col)));
         end loop;
         Result (row) := Sum;
      end loop;

      return Result;

   end Dot;

   --  ----------------------------------------------------------------------------

   function Dot (L : Real_Float_Vector; R : Integer_Matrix)
                 return Real_Float_Vector is
      Sum    : Float;
      Result : Real_Float_Vector (L'Range);
   begin
      Assert (L'Length = R'Length (2), "Dot, L'Length /= R'Length (2)");
      for row in R'Range loop
         Sum := 0.0;
         for col in L'Range loop
            Sum := Sum + L (col) * Float (R (row, col));
            Assert (Sum'Valid, "Dot, Sum =" & Float'Image (Sum) & "row, col:"
                    & Integer'Image (row) & ", " & Integer'Image (col) &
                      ", L, R:" & Float'Image (L (col)) & ", " &
                      Integer'Image (R (row, col)));
         end loop;

         Result (row) := Sum;
      end loop;

      return Result;

   end Dot;

   --  ----------------------------------------------------------------------------

   function Dot (L, R : Real_Float_List) return Float is
      Result : Float := 0.0;
   begin
      for index in L.First_Index .. L.Last_Index loop
         Result := Result + L.Element (index) * R.Element (index);
      end loop;

      return Result;

   end Dot;

   --  ----------------------------------------------------------------------------

   function Exp (M : Real_Float_Matrix) return Real_Float_Matrix is
      use Maths.Long_Float_Math_Functions;
      Value  : Long_Float;
      Result : Real_Float_Matrix (M'Range, M'Range (2));
   begin
      for row in M'Range loop
         for col in M'Range (2) loop
            Value := Exp (Long_Float (M (row, col)));
            if Value < Float'Safe_Last - 1.0 then
               Result (row, col) := Float (Value);
            else
               Result (row, col) := Float'Safe_Last - 1.0;
            end if;
         end loop;
      end loop;

      return Result;

   end Exp;

   --  ------------------------------------------------------------------------

   function Exp (V : Real_Float_Vector) return Real_Float_Vector is
      use Maths.Long_Float_Math_Functions;
      Value  : Long_Float;
      Result : Real_Float_Vector (V'Range);
   begin
      for index in V'Range loop
         Value := Exp (Long_Float (V (index)));
         if Value < Float'Safe_Last - 1.0 then
            Result (index) := Float (Value);
         else
            Result (index) := Float'Safe_Last - 1.0;
         end if;
      end loop;

      return Result;

   end Exp;

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

   function Flatten (M : Real_Float_Matrix) return Real_Float_Vector is
      Flat : Real_Float_Vector (1 .. M'Length * M'Length (2));
   begin
      for row in M'Range loop
         for col in M'Range (2) loop
            Flat ((row - 1) * M'Length (2) + col) := M (row, col);
         end loop;
      end loop;

      return Flat;

   end Flatten;

   --  -------------------------------------------------------------------------

   function Get_Row (Matrix : Binary_Matrix; Row : Integer)
                     return Binary_Array is
      Result : Binary_Array (Matrix'Range (2));
   begin
      for col in Matrix'Range (2) loop
         Result (col) := Matrix  (Row, col);
      end loop;

      return Result;

   end Get_Row;

   --  ------------------------------------------------------------------------

   function Get_Row (Matrix : Integer_Matrix; Row : Integer)
                     return Integer_Array is
      Result : Integer_Array (Matrix'Range (2));
   begin
      for col in Matrix'Range (2) loop
         Result (col) := Matrix  (Row, col);
      end loop;

      return Result;

   end Get_Row;

   --  ------------------------------------------------------------------------

   function Get_Row (Matrix : Real_Float_Matrix; Row : Integer)
                     return Real_Float_Vector is
      Result : Real_Float_Vector (Matrix'Range (2));
   begin
      for col in Matrix'Range (2) loop
         Result (col) := Matrix  (Row, col);
      end loop;

      return Result;

   end Get_Row;

   --  ------------------------------------------------------------------------
   --  Hadamard product
   function H_Product (L, R : Binary_Array) return Binary_Array is
      Product : Binary_Array (L'Range);
   begin
      Assert (L'Length = R'Length, "H_Product, L and R lengths, " &
                Integer'Image (L'Length) & "," & Integer'Image (R'Length) &
                " are different.");
      for index in L'Range loop
         Product (index) := L (index) * R (index);
      end loop;

      return Product;

   end H_Product;

   --  ----------------------------------------------------------------------------

   function H_Product (L, R : Real_Float_Vector) return Real_Float_Vector is
      Product : Real_Float_Vector (L'Range);
   begin
      Assert (L'Length = R'Length, "H_Product, L and R lengths, " &
                Integer'Image (L'Length) & "," & Integer'Image (R'Length) &
                " are different.");
      for index in L'Range loop
         Product (index) := L (index) * R (index);
      end loop;

      return Product;

   end H_Product;

   --  ----------------------------------------------------------------------------

   function H_Product (L : Real_Float_Vector; R : Real_Float_List)
                       return Real_Float_Vector is
      Product : Real_Float_Vector (L'Range);
   begin
      Assert (L'Length = Integer (R.Length), "H_Product, L and R lengths, " &
                Integer'Image (L'Length) & "," &
                Integer'Image (Integer (R.Length)) & " are different.");
      for index in L'Range loop
         Product (index) := L (index) * R (index);
      end loop;

      return Product;

   end H_Product;

   --  ----------------------------------------------------------------------------

   function H_Product (L : Real_Float_List; R : Real_Float_Vector)
                       return Real_Float_Vector is
      Product : Real_Float_Vector (R'Range);
   begin
      Assert (Integer (L.Length) = R'Length, "H_Product, L and R lengths, " &
                Integer'Image (Integer (L.Length)) & "," &
                Integer'Image (R'Length) & " are different.");
      for index in R'Range loop
         Product (index) := L (index) * R (index);
      end loop;

      return Product;

   end H_Product;

   --  ----------------------------------------------------------------------------

   function H_Product (L, R : Real_Float_List) return Real_Float_Vector is
      use Ada.Containers;
      Product : Real_Float_Vector (1 .. Integer (L.Length));
   begin
      Assert (L.Length = R.Length, "H_Product, L and R have different lengths");
      for index in L.First_Index .. L.Last_Index loop
         Product (index) := L (index) * R (index);
      end loop;

      return Product;

   end H_Product;

   --  ----------------------------------------------------------------------------

   function H_Product (L, R : Real_Float_Matrix) return Real_Float_Matrix is
      Product : Real_Float_Matrix (L'Range, L'Range (2));
   begin
      Assert (L'Length = R'Length, "H_Product, L and R lengths, " &
                Integer'Image (L'Length) & "," & Integer'Image (R'Length) &
                " are different.");
      Assert (L'Length (2) = R'Length (2), "H_Product, L and R widths, " &
                Integer'Image (L'Length (2)) & "," &
                Integer'Image (R'Length (2)) & " are different.");
      for row in L'Range loop
         for col in L'Range (2) loop
            Product (row, col) := L (row, col) * R (row, col);
         end loop;
      end loop;

      return Product;

   end H_Product;

   --  ----------------------------------------------------------------------------

   function Log (V : Real_Float_Vector) return Real_Float_Vector is
      use Maths.Float_Math_Functions;
      Result : Real_Float_Vector (V'Range);
   begin
      for index in V'Range loop
         Result (index) := Log (V (index));
      end loop;

      return Result;

   end Log;

   --  ------------------------------------------------------------------------

   function Max_Vec (L : Float; R : Real_Float_Vector) return Real_Float_Vector is
      Result : Real_Float_Vector (R'Range);
   begin
      for row in R'Range loop
         Result (row) := Float'Max (L, R (row));
      end loop;

      return Result;

   end Max_Vec;

   --  ------------------------------------------------------------------------

   function Multiply_Elements (L, R : Real_Float_Matrix) return Real_Float_Matrix is
      Result : Real_Float_Matrix := L;
   begin
      for row in L'Range loop
         for col in L'Range (2) loop
            Result (row, col) := Result (row, col) * R (row, col);
         end loop;
      end loop;

      return Result;

   end Multiply_Elements;

   --  ------------------------------------------------------------------------

   function Norm (M : Real_Float_List) return Float is
      use Maths.Float_Math_Functions;
      Sum : Float := 0.0;
   begin
      for row in M.First_Index .. M.Last_Index loop
         Sum := Sum + M (row) ** 2;
      end loop;

      return Sqrt (Sum);

   end Norm;

   --  ------------------------------------------------------------------------

   function Norm (M : Real_Float_Vector) return Float is
      use Maths.Float_Math_Functions;
      Sum : Float := 0.0;
   begin
      for row in M'Range loop
         Sum := Sum + M (row) ** 2;
      end loop;

      return Sqrt (Sum);

   end Norm;

   --  ------------------------------------------------------------------------

   function Normalize (M : Float_Array) return Float_Array is
      Sum       : Float := 0.0;
      Recip_Sum : Float;
      Result    : Float_Array (M'Range);
   begin
      for row in M'Range loop
         Sum := Sum + M (row);
      end loop;

      Recip_Sum := 1.0 /Sum;
      for row in M'Range loop
         Result (row) := Recip_Sum * M (row);
      end loop;

      return Result;

   end Normalize;

   --  ------------------------------------------------------------------------

   function Normalize_Rows (M : Real_Float_Matrix) return Real_Float_Matrix is
      Result : Real_Float_Matrix (M'Range, M'Range (2));
   begin
      for row in M'Range loop
         declare
            M_Row : Float_Array (M'Range (2));
         begin
            for col in M_Row'Range loop
               M_Row (col) := M (row, col);
            end loop;
            M_Row := Normalize (M_Row);
            for col in M_Row'Range loop
               Result (row, col) := M_Row (col);
            end loop;
         end;
      end loop;

      return Result;

   end Normalize_Rows;

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

   function Max (V : Integer_Array) return Integer is
      Result : Integer := Integer'First;
   begin
      for index in V'Range loop
         if V (index) > Result then
            Result := V (index);
         end if;
      end loop;

      return Result;

   end Max;

   --  ------------------------------------------------------------------------

   function Max (Data : Real_Float_Matrix) return Float is
      Max_Val : Float := Float'Safe_First;
   begin
      for row in Data'Range loop
         for col in Data'Range (2) loop
            if Data (row, col) > Max_Val then
               Max_Val := Data (row, Col);
            end if;
         end loop;
      end loop;

      return Max_Val;

   end Max;

   --  ------------------------------------------------------------------------
   --  Max returns a vector containing the maximum value of each row of a matrix
   function Max (Data : Real_Float_Matrix) return Real_Float_Vector is
      Result  : Real_Float_Vector (Data'Range);
      Max_Val : Float := Float'Safe_First;
   begin
      for row in Data'Range loop
         for col in Data'Range (2) loop
            if Data (row, col) > Max_Val then
               Max_Val := Data (row, Col);
            end if;
         end loop;
         Result (row) := Max_Val;
      end loop;

      return Result;

   end Max;

   --  ------------------------------------------------------------------------

   function Max (L, R : Real_Float_Vector) return Real_Float_Vector is
      Result  : Real_Float_Vector (L'Range);
   begin
      for row in L'Range loop
         if L (row) >= R (row) then
            Result (row) := L (row);
         else
            Result (row) := R (row);
         end if;
      end loop;

      return Result;

   end Max;

   --  ------------------------------------------------------------------------

   function Max (V : Real_Float_Vector) return Float is
      Result : Float := Float'Safe_First;
   begin
      for index in V'Range loop
         if V (index) > Result then
            Result := V (index);
         end if;
      end loop;

      return Result;

   end Max;

   --  ------------------------------------------------------------------------

   function Min (Data : Real_Float_Matrix) return Float is
      Min_Val : Float := Float'Safe_Last;
   begin
      for row in Data'Range loop
         for col in Data'Range (2) loop
            if Data (row, col) < Min_Val then
               Min_Val := Data (row, Col);
            end if;
         end loop;
      end loop;

      return Min_Val;

   end Min;

   --  ------------------------------------------------------------------------

   function Min (L, R : Real_Float_Vector) return Real_Float_Vector is
      Result  : Real_Float_Vector (L'Range);
   begin
      for row in L'Range loop
         if L (row) < R (row) then
            Result (row) := L (row);
         else
            Result (row) := R (row);
         end if;
      end loop;

      return Result;

   end Min;

   --  ------------------------------------------------------------------------

   function Min (V : Real_Float_Vector) return Float is
      Result : Float := Float'Safe_Last;
   begin
      for index in V'Range loop
         if V (index) < Result then
            Result := V (index);
         end if;
      end loop;

      return Result;

   end Min;

   --  ------------------------------------------------------------------------

   function Outer (L, R : Real_Float_Vector) return Real_Float_Vector is
      Result : Real_Float_Vector := L;
   begin
      for row in Result'Range loop
         Result (row) := Result (row) * R  (row);
      end loop;

      return Result;

   end Outer;

   --  ------------------------------------------------------------------------

   function Slice (Matrix : Binary_Matrix; First, Last : Positive)
                   return Binary_Matrix is
      Result : Binary_Matrix (1 .. Last - First + 1, Matrix'Range (2));
   begin
      for row in First .. Last loop
         for col in Result'Range (2) loop
            Result (row - First + 1, col) := Matrix  (row, col);
         end loop;
      end loop;

      return Result;

   end Slice;

   --  ------------------------------------------------------------------------

   function Slice (Matrix : Real_Float_Matrix; First, Last : Positive)
                   return Real_Float_Matrix is
      Result : Real_Float_Matrix (1 .. Last - First + 1, Matrix'Range (2));
   begin
      for row in First .. Last loop
         for col in Result'Range (2) loop
            Result (row - First + 1, col) := Matrix  (row, col);
         end loop;
      end loop;

      return Result;

   end Slice;

   --  ------------------------------------------------------------------------

   function Slice (Matrix : Integer_Matrix; First, Last : Positive)
                   return Integer_Matrix is
      Result : Integer_Matrix (1 .. Last - First + 1, Matrix'Range (2));
   begin
      for row in First .. Last loop
         for col in Result'Range (2) loop
            Result (row - First + 1, col) := Matrix  (row, col);
         end loop;
      end loop;

      return Result;

   end Slice;

   --  ------------------------------------------------------------------------

   function Sum (Data : Integer_Matrix) return Integer is
      Result : Integer := 0;
   begin
      for row in Data'Range loop
         for col in Data'Range (2) loop
            Result := Result + Data (row, col);
         end loop;
      end loop;

      return Result;

   end Sum;

   --  ------------------------------------------------------------------------

   function Sum (Data : Real_Float_Matrix) return Float is
      Result : Float := 0.0;
   begin
      for row in Data'Range loop
         for col in Data'Range (2) loop
            Result := Result + Data (row, Col);
         end loop;
      end loop;

      return Result;

   end Sum;

   --  ------------------------------------------------------------------------

   function Sum (Data : Real_Float_Matrix) return Real_Float_Vector is
      Val    : Float;
      Result : Real_Float_Vector (Data'Range);
   begin
      for row in Data'Range loop
         Val := 0.0;
         for col in Data'Range (2) loop
            Val := Val + Data (row, Col);
         end loop;
         Result (row) := Val;
      end loop;

      return Result;

   end Sum;

   --  ------------------------------------------------------------------------

   function Sum (Data : Real_Float_Vector) return Float is
      Result : Float := 0.0;
   begin
      for index in Data'Range loop
         Result := Result + Data (index);
      end loop;

      return Result;

   end Sum;

   --  ------------------------------------------------------------------------

   function Sum_Diagonal (Data : Integer_Matrix) return Integer is
      Result : Integer := 0;
   begin
      for row in Data'Range loop
         Result := Result + Data (row, row);
      end loop;

      return Result;

   end Sum_Diagonal;

   --  ------------------------------------------------------------------------

   function Sum_Each_Column (Data : Real_Float_Matrix)
                             return Real_Float_Vector is
      Sum    : Float;
      Result : Real_Float_Vector (Data'Range (2));
   begin
      for col in Data'Range (2) loop
         Sum := 0.0;
         for row in Data'Range loop
            Sum := Sum + Data (row, Col);
         end loop;
         Result (col) := Sum;
      end loop;

      return Result;

   end Sum_Each_Column;

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

   function To_Boolean_Array_Of_Lists (List : NL_Types.Boolean_List_2D)
                                       return NL_Types.Boolean_Array_Of_Lists is
      use NL_Types;
   begin
      if not List.Is_Empty then
         declare
            Result : Boolean_Array_Of_Lists
              (List.First_Index .. List.Last_Index);
         begin
            for row in Result'Range loop
               Result (row) := List (row);
            end loop;
            return Result;
         end;
      else
         declare
            Result : Boolean_Array_Of_Lists (1 .. 0);
         begin
            return Result;
         end;
      end if;

   end To_Boolean_Array_Of_Lists;

   --  ------------------------------------------------------------------------

   function To_Boolean_Matrix (List : NL_Types.Boolean_List_2D)
                               return Boolean_Matrix is
      function Length_2 return Positive is
         Row_Length : Positive;
         Max        : Positive := 1;
      begin
         for row in List.First_Index .. List.Last_Index loop
            Row_Length := Positive (List (row).Length);
            if Row_Length > Max then
               Max := Row_Length;
            end if;
         end loop;
         return Max;
      end Length_2;

      List_1D                                     : NL_Types.Boolean_List;
      Result                                      : Boolean_Matrix
        (1 .. Integer (List.Length), 1 .. Length_2) :=
                                                      (others => (others => False));
   begin
      for row in Result'Range loop
         List_1D := List (row);
         for col in List_1D.First_Index .. List_1D.Last_Index loop
            Result (row, col) := List_1D (col);
         end loop;
      end loop;

      return Result;

   end To_Boolean_Matrix;

   --  ------------------------------------------------------------------------

   function To_Boolean_Matrix (IM : Integer_Matrix) return Boolean_Matrix is
      Result : Boolean_Matrix (IM'Range, IM'Range (2));
   begin
      for row in Result'Range loop
         for col in Result'Range (2) loop
            Result (row, col) := IM (row, col) /= 0;
         end loop;
      end loop;

      return Result;

   end To_Boolean_Matrix;

   --  ------------------------------------------------------------------------

   function To_Real_Float_List (V : Real_Float_Vector) return Real_Float_List is
      F_List : Real_Float_List;
   begin
      for index in V'Range loop
         F_List.Append (V (index));
      end loop;

      return F_List;

   end To_Real_Float_List;

   --  -------------------------------------------------------------------------

   function To_Real_Float_List (M : Real_Float_Matrix; Row : Integer)
                                return Real_Float_List is
      F_List : Real_Float_List;
   begin
      for index in M'Range (2) loop
         F_List.Append (M (Row, index));
      end loop;

      return F_List;

   end To_Real_Float_List;

   --  -------------------------------------------------------------------------

   function To_Real_Float_Matrix (Matrix : Binary_Matrix)
                                  return Real_Float_Matrix is
      Result : Real_Float_Matrix (Matrix'Range, Matrix'Range (2));
   begin
      for row in Matrix'Range loop
         for col in Matrix'Range (2) loop
            Result (row, col) := Float (Matrix  (row, col));
         end loop;
      end loop;

      return Result;

   end To_Real_Float_Matrix;

   --  ------------------------------------------------------------------------

   function To_Real_Float_Matrix (List : NL_Types.Float_List_2D)
                                  return Real_Float_Matrix is
   begin
      if not List.Is_Empty then
         declare
            List_Row : NL_Types.Float_List;
            Result   : Real_Float_Matrix
              (List.First_Index .. List.Last_Index,
               List (1).First_Index .. List (1).Last_Index);
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
            Result : Real_Float_Matrix (1 .. 0, 1 .. 0);
         begin
            return Result;
         end;
      end if;

   end To_Real_Float_Matrix;

   --  ------------------------------------------------------------------------

   function To_Real_Float_Matrix
     (IA : Integer_Array; Data_Axis : Positive := 1)
      return Real_Float_Matrix is
      Routine_Name : constant String :=
                       "ML_Arrays_And_Matrices.To_Real_Float_Matrix Integer_Array ";
   begin
      Assert (Data_Axis = 1 or Data_Axis = 2, Routine_Name &
                " Invalid Data_Axis" & Integer'Image (Data_Axis) &
                " should be 1 or 2.");
      if Data_Axis = 1 then
         declare
            Result : Real_Float_Matrix (IA'Range, 1 .. 1);
         begin
            for row in IA'Range loop
               Result (row, 1) := Float (IA (row));
            end loop;
            return Result;
         end;

      else
         declare
            Result : Real_Float_Matrix (1 .. 1, IA'Range);
         begin
            for col in IA'Range loop
               Result (1, col) := Float (IA (col));
            end loop;
            return Result;
         end;
      end if;

   end To_Real_Float_Matrix;

   --  ------------------------------------------------------------------------

   function To_Real_Float_Matrix
     (List        : Real_Float_List; Data_Axis : Positive := 1;
      First_Index : Integer := 1)
      return Real_Float_Matrix is
      Routine_Name : constant String :=
                       "ML_Arrays_And_Matrices.To_Real_Float_Matrix ";
   begin
      Assert (Data_Axis = 1 or Data_Axis = 2, Routine_Name &
                " Invalid Data_Axis" & Integer'Image (Data_Axis) &
                " should be 1 or 2.");
      if not List.Is_Empty then
         if Data_Axis = 1 then
            declare
               Result : Real_Float_Matrix
                 (First_Index .. First_Index + Integer (List.Length) - 1, 1 .. 1);
            begin
               for row in Result'Range loop
                  Result (row, 1) := List (row);
               end loop;
               return Result;
            end;

         else
            declare
               Result : Real_Float_Matrix
                 (1 .. 1,
                  First_Index .. First_Index + Integer (List.Length) - 1);
            begin
               for col in Result'Range (2) loop
                  Result (1, col) := List (col);
               end loop;
               return Result;
            end;
         end if;

      else
         Put_Line (Routine_Name & "empty list.");
         declare
            Result : Real_Float_Matrix (1 .. 0, 1 .. 0);
         begin
            return Result;
         end;
      end if;

   end To_Real_Float_Matrix;

   --  ------------------------------------------------------------------------

   function To_Real_Float_Matrix
     (Vec         : Real_Float_Vector; Data_Axis : Positive := 1;
      First_Index : Integer := 1) return Real_Float_Matrix is
      Routine_Name : constant String :=
                       "ML_Arrays_And_Matrices.To_Real_Float_Matrix Real_Float_Vector ";
   begin
      Assert (Data_Axis = 1 or Data_Axis = 2, Routine_Name &
                " Invalid Data_Axis" & Integer'Image (Data_Axis) &
                " should be 1 or 2.");
         if Data_Axis = 1 then
            declare
               Result : Real_Float_Matrix
                 (First_Index .. First_Index + Vec'Length - 1, 1 .. 1);
            begin
               for row in Result'Range loop
                  Result (row, 1) := Vec (row);
               end loop;
               return Result;
            end;

         else
            declare
               Result : Real_Float_Matrix
                 (1 .. 1, First_Index .. First_Index + Vec'Length - 1);
            begin
               for col in Result'Range (2) loop
                  Result (1, col) := Vec (col);
               end loop;
               return Result;
            end;
         end if;

   end To_Real_Float_Matrix;

   --  ------------------------------------------------------------------------

   function To_Real_Float_Matrix (List : Float_Vector_List)
                                  return Real_Float_Matrix is
   begin
      if not List.Is_Empty then
         declare
            List_Row : Real_Float_Vector (List.Element (1)'range);
            Result   : Real_Float_Matrix
              (List.First_Index .. List.Last_Index, List_Row'range);
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
            Result : Real_Float_Matrix (1 .. 0, 1 .. 0);
         begin
            return Result;
         end;
      end if;

   end To_Real_Float_Matrix;

   --  ------------------------------------------------------------------------

   function To_Real_Float_Matrix (List : Real_Float_List_2D)
                                  return Real_Float_Matrix is
   begin
      if not List.Is_Empty then
         declare
            List_Row : Real_Float_List;
            Result   : Real_Float_Matrix
              (List.First_Index .. List.Last_Index,
               List (1).First_Index .. List (1).Last_Index);
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
            Result : Real_Float_Matrix (1 .. 0, 1 .. 0);
         begin
            return Result;
         end;
      end if;

   end To_Real_Float_Matrix;

   --  ------------------------------------------------------------------------

   function To_Integer_Array (List : ML_Types.Integer_List)
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

   function To_Integer_Matrix (List : ML_Types.Integer_List_2D)
                               return Integer_Matrix is
   begin
      if not List.Is_Empty then
         declare
            List_Row : ML_Types.Integer_List;
            Result   : Integer_Matrix
              (List.First_Index .. List.Last_Index,
               List (1).First_Index .. List (1).Last_Index);
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
            Result : Integer_Matrix (1 .. 0, 1 .. 0);
         begin
            return Result;
         end;
      end if;

   end To_Integer_Matrix;

   --  ------------------------------------------------------------------------

   function To_Integer_Matrix (List : ML_Types.Value_Data_Lists_2D)
                               return Integer_Matrix is
      use ML_Types;
   begin
      Assert (List.Element (1).Element (1).Value_Kind = Integer_Type,
              " To_Integer_Matrix List type is not integer.");
      if not List.Is_Empty then
         declare
            List_Row : Value_Data_List;
            Result   : Integer_Matrix
              (List.First_Index .. List.Last_Index,
               List (1).First_Index .. List (1).Last_Index);
         begin
            for row in Result'Range loop
               List_Row := List (row);
               for col in Result'Range (2) loop
                  case List_Row (col).Value_Kind is
                     when Boolean_Type =>
                        if List_Row.Element (col).Boolean_Value then
                           Result (row, col) := 1;
                        else
                           Result (row, col) := 0;
                        end if;
                     when Float_Type =>
                        Result (row, col) :=
                          Integer (List_Row.Element (col).Float_Value);
                     when Integer_Type =>
                        Result (row, col) := List_Row (col).Integer_Value;
                     when UB_String_Type => Result (row, col) := 0;
                  end case;
               end loop;
            end loop;
            return Result;
         end;
      else
         declare
            Result : Integer_Matrix (1 .. 0, 1 .. 0);
         begin
            return Result;
         end;
      end if;

   end To_Integer_Matrix;

   --  ------------------------------------------------------------------------

   function To_Integer_Matrix (Bin : Binary_Matrix) return Integer_Matrix is
      Result : Integer_Matrix (Bin'Range, Bin'Range (2));
   begin
      for row in Bin'Range loop
         for col in Bin'Range (2) loop
            Result (row, col) := Bin (row, col);
         end loop;
      end loop;

      return Result;

   end To_Integer_Matrix;

   --  ------------------------------------------------------------------------

   function To_Integer_Matrix (Bool : Boolean_Matrix) return Integer_Matrix is
      Result   : Integer_Matrix (Bool'Range, Bool'Range (2)) :=
                   (others => (others => 0));
   begin
      for row in Bool'Range loop
         for col in Bool'Range (2) loop
            if Bool (row, col) then
               Result (row, col) := 1;
            end if;
         end loop;
      end loop;

      return Result;

   end To_Integer_Matrix;

   --  ------------------------------------------------------------------------

   function To_Integer_Matrix (IA : Integer_Array) return Integer_Matrix is
      Result : Integer_Matrix (IA'Range, 1 .. 1);
   begin
      for row in IA'Range loop
         Result (row, 1) := IA (row);
      end loop;

      return Result;

   end To_Integer_Matrix;

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

   function To_Real_Float_Matrix (BM : Boolean_Matrix)
                                  return Real_Float_Matrix is
      Result : Real_Float_Matrix (BM'Range, BM'Range (2));
   begin
      for row in BM'Range loop
         for col in BM'Range (2) loop
            if BM (row, col) then
               Result (row, col) := 1.0;
            else
               Result (row, col) := 0.0;
            end if;
         end loop;
      end loop;

      return Result;

   end To_Real_Float_Matrix;

   --  ------------------------------------------------------------------------

   function To_Real_Float_Matrix (IM : Integer_Matrix)
                                  return Real_Float_Matrix is
      Result : Real_Float_Matrix (IM'Range, IM'Range (2));
   begin
      for row in IM'Range loop
         for col in IM'Range (2) loop
            Result (row, col) := Float (IM (row, col));
         end loop;
      end loop;

      return Result;

   end To_Real_Float_Matrix;

   --  ------------------------------------------------------------------------

   function To_Real_Float_Vector (List : Binary_Array)
                                  return Real_Float_Vector is
      Vec : Real_Float_Vector (List'Range);
   begin
      for index in List'Range loop
         Vec (index) := Float (List (index));
      end loop;

      return Vec;

   end To_Real_Float_Vector;

   --  ------------------------------------------------------------------------

   function To_Real_Float_Vector (List : Integer_Array)
                                  return Real_Float_Vector is
      Vec : Real_Float_Vector (List'Range);
   begin
      for index in List'Range loop
         Vec (index) := Float (List (index));
      end loop;

      return Vec;

   end To_Real_Float_Vector;

   --  ------------------------------------------------------------------------

   function To_Real_Float_Vector (List : Real_Float_List)
                                  return Real_Float_Vector is
      Vec : Real_Float_Vector (1 .. Positive (List.Length));
   begin
      for index in List.First_Index .. List.Last_Index loop
         Vec (index) := List (index);
      end loop;

      return Vec;

   end To_Real_Float_Vector;

   --  ------------------------------------------------------------------------

   function To_Unbound_Array (UB_List : ML_Types.Unbounded_List)
                              return Unbounded_String_Array is
   begin
      if not UB_List.Is_Empty then
         declare
            Result : Unbounded_String_Array
              (UB_List.First_Index .. UB_List.Last_Index);
         begin
            for row in Result'Range loop
               Result (row) := UB_List (row);
            end loop;
            return Result;
         end;

      else
         declare
            Result : Unbounded_String_Array (1 .. 0);
         begin
            return Result;
         end;
      end if;

   end To_Unbound_Array;

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

   function Unit_Integer_Matrix (Num_Rows : Positive) return Integer_Matrix is
      Mat : Integer_Matrix (1 .. Num_Rows, 1 .. Num_Rows) :=
              (others => (others => 0));
   begin
      for row in Mat'Range loop
         Mat (row, row) := 1;
      end loop;

      return Mat;

   end Unit_Integer_Matrix;

   --  -------------------------------------------------------------------------

   function Unit_Float_Matrix (Num_Rows : Positive) return Real_Float_Matrix is
      Mat : Real_Float_Matrix (1 .. Num_Rows, 1 .. Num_Rows) :=
              (others => (others => 0.0));
   begin
      for row in Mat'Range loop
         Mat (row, row) := 1.0;
      end loop;

      return Mat;

   end Unit_Float_Matrix;

   --  -------------------------------------------------------------------------

   function Zero_Array (Num_Rows : Positive)
                        return Real_Float_Vector is
      Loaded : Real_Float_Vector (1 .. Num_Rows);
   begin
      for row in Loaded'Range loop
         Loaded (row) := 0.0;
      end loop;

      return Loaded;

   end Zero_Array;

   --  -------------------------------------------------------------------------

   function Zero_Matrix (Num_Rows, Num_Cols : Positive) return Binary_Matrix is
      Loaded : Binary_Matrix (1 .. Num_Rows, 1 .. Num_Cols);
   begin
      for row in Loaded'Range loop
         for col in Loaded'Range (2) loop
            Loaded (row, col) := 0;
         end loop;
      end loop;

      return Loaded;

   end Zero_Matrix;

   --  -------------------------------------------------------------------------

   function Zero_Matrix (Num_Rows, Num_Cols : Positive) return Integer_Matrix is
      Loaded : Integer_Matrix (1 .. Num_Rows, 1 .. Num_Cols);
   begin
      for row in Loaded'Range loop
         for col in Loaded'Range (2) loop
            Loaded (row, col) := 0;
         end loop;
      end loop;

      return Loaded;

   end Zero_Matrix;

   --  -------------------------------------------------------------------------

   function Zero_Matrix (Num_Rows, Num_Cols : Positive)
                         return Real_Float_Matrix is
      Loaded : Real_Float_Matrix (1 .. Num_Rows, 1 .. Num_Cols);
   begin
      for row in Loaded'Range loop
         for col in Loaded'Range (2) loop
            Loaded (row, col) := 0.0;
         end loop;
      end loop;

      return Loaded;

   end Zero_Matrix;

   --  -------------------------------------------------------------------------

end ML_Arrays_And_Matrices;
