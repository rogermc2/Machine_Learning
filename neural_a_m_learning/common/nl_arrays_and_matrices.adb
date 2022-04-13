
--  with Ada.Assertions; use Ada.Assertions;

package body NL_Arrays_And_Matrices is

    function "*" (L : Float; R : Float_Matrix) return Float_Matrix is
        Result : Float_Matrix (R'First .. R'Last, R'First (2) .. R'Last (2));
    begin
        for row in R'First .. R'Last loop
            for col in R'First (2) .. R'Last (2) loop
                Result (row, col) := L * R (row, col);
            end loop;
        end loop;

        return Result;

    end "*";

    --  ------------------------------------------------------------------------

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

    function "**" (L : Float_Array; P : Integer) return Float_Array is
        Result : Float_Array := L;
    begin
        for row in Result'First .. Result'Last loop
            Result (row) := Result (row) ** P;
        end loop;

        return Result;

    end "**";

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

    function "+" (L, R : Float_Array) return Float_Array is
        Result : Float_Array (L'First .. L'Last);
    begin
        for row in L'First .. L'Last loop
            Result (row) := L (row) + R (row);
        end loop;

        return Result;

    end "+";

    --  ------------------------------------------------------------------------

    function "+" (L, R : Float_Matrix) return Float_Matrix is
        Result : Float_Matrix (L'First .. L'Last, L'First (2) .. L'Last (2));
    begin
        for row in L'First .. L'Last loop
            for col in L'First (2) .. L'Last (2) loop
                Result (row, col) := L (row, col) + R (row, col);
            end loop;
        end loop;

        return Result;

    end "+";

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

    --  ----------------------------------------------------------------------------
    --  For A = [a(1,1) ... a(m,n)] and B = [b(1,1) ... b(n,p)]
    --  A.B = [c(i,j)] where
    --  c(i,j) = a(i,1)*b(1,j) + ... a(i,n)*b(n,j)
    function Dot (L, R : Float_Matrix) return Float_Matrix is
--          Routine_Name : constant String := "NL_Arrays_And_Matrices.Dot ";
        Num_Rows : constant Positive := L'Length;
        Num_Cols : constant Positive := R'Length (2);
        Product  : Float_Matrix  (1 .. Num_Rows, 1 .. Num_Cols);
    begin
--          Assert (R'Length = L'Length (2), Routine_Name &
--                    "Num rows" & Integer'Image (R'Length) & " of right matrix " &
--                    "doesn't equal num colums" & Integer'Image (L'Length (2)) &
--                    " of left matrix");
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

    --  ----------------------------------
    function To_Float_Array (List : NL_Types.Float_List) return Float_Array is
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
