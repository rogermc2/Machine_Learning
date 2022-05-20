
with Ada.Assertions; use Ada.Assertions;
--  with Ada.Text_IO; use Ada.Text_IO;

with Maths;

package body NL_Arrays_And_Matrices is

   --  ------------------------------------------------------------------------

   function "/" (L : Real_Float_Matrix; R : Real_Float_Vector)
                  return Real_Float_Matrix is
      Result : Real_Float_Matrix  := L;
   begin
      for row in L'Range loop
         for col in L'Range (2) loop
            Result (row, col) := Result (row, col) / R (col);
         end loop;
      end loop;

      return Result;

   end "/";

   --  ------------------------------------------------------------------------

   function "+" (L : Real_Float_Matrix; R : Real_Float_Vector)
                  return Real_Float_Matrix is
      Result : Real_Float_Matrix  := L;
   begin
      for row in L'Range loop
         for col in L'Range (2) loop
            Result (row, col) := Result (row, col) + R (col);
         end loop;
      end loop;

      return Result;

   end "+";

   --  ------------------------------------------------------------------------

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
      Result : Real_Float_Matrix := L;
   begin
      for row in L'Range loop
         for col in L'Range (2) loop
            Result (row, col) := Result (row, col) - R (row);
         end loop;
      end loop;

      return Result;

   end "-";

   --  ------------------------------------------------------------------------

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
      use Maths.Float_Math_Functions;
      Result : Real_Float_Matrix (M'Range, M'Range (2));
   begin
      for row in M'Range loop
         for col in M'Range (2) loop
            Result (row, col) := Exp (M (row, col));
         end loop;
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
   --  Max returns a vector containing the maximum value of each matrix
   function Max (Data : Real_Float_Matrix) return Real_Float_Vector is
      Result  : Real_Float_Vector (Data'Range);
      Max_Val : Float;
   begin
      for row in Data'Range loop
         Max_Val := Data (row, 1);
         for col in 2 .. Data'Length (2) loop
            if Data (row, col) > Max_Val then
               Max_Val := Data (row, Col);
            end if;
         end loop;
         Result (row) := Max_Val;
      end loop;

      return Result;

   end Max;

   --  ------------------------------------------------------------------------

   function Sum (Data : Real_Float_Matrix) return Real_Float_Vector is
      Result : Real_Float_Vector (Data'Range);
      Val    : Float;
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

   function To_Real_Float_Matrix (List : NL_Types.Float_List_2D)
                                   return Real_Float_Matrix is
   begin
      if not List.Is_Empty then
         declare
            List_Row : NL_Types.Float_List;
            Result   : Real_Float_Matrix
              (List.First_Index .. List.Last_Index,
               List (1).First_Index ..List (1).Last_Index);
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

   function To_Real_Float_Matrix (IM : Integer_Matrix)
                                   return Real_Float_Matrix is
      Result : Real_Float_Matrix (1 .. IM'Length, 1 .. IM'Length (2));
   begin
      for row in IM'First .. IM'Last loop
         for col in IM'First (2) .. IM'Last (2) loop
            Result (row, col) := Float (IM (row, col));
         end loop;
      end loop;

      return Result;

   end To_Real_Float_Matrix;

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
