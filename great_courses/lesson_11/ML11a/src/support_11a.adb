
with Maths;

package body Support_11A is

   function Loss (Values : Real_Float_Matrix; Min_Indices : out Integer_Array)
                  return Float;
   function Means (Data : Float_Array_List) return Real_Float_Matrix;

   --  ------------------------------------------------------------------------

   function Assign_Data (Data, Centres : Real_Float_Matrix;
                         Centre_Ids    : out Integer_Array) return Float is
      Diff     : array (Centres'Range, Data'Range, Data'Range (2)) of Float;
      Diff2    : array (Centres'Range, Data'Range, Data'Range (2)) of Float;
      Total    : Real_Float_Matrix (Centres'Range, Data'Range) :=
                   (others => (others => 0.0));
   begin
      for d3 in Diff'Range loop
         for row in Data'Range loop
            for col in Data'Range (2) loop
               Diff (d3, row, col) := Data (row, col) - Centres (d3, col);
               Diff2 (d3, row, col) := Diff (d3, row, col) ** 2;
            end loop;
         end loop;
      end loop;

      for d3 in Diff2'Range loop
         for row in Total'Range loop
            for col in Total'Range (2) loop
               Total (row, col) := Total (row, col) + Diff2  (d3, row, col);
            end loop;
         end loop;
      end loop;

      return Loss (Total, Centre_Ids);

   end Assign_Data;

   --  ------------------------------------------------------------------------

   function Compute_Means (Data       : Real_Float_Matrix;
                           Centre_Ids : Integer_Array; K : Positive)
                           return Real_Float_Matrix is
      Centres    : Real_Float_Matrix (1 .. K, Data'Range (2)) :=
                     (others => (others => 0.0));
      aCol       : Float_Array (Data'Range (2));
      Cols       : Float_Array_List;

   begin
      for index in 1 .. K loop
         Cols.Clear;
         for row in Data'Range loop
            if Centre_Ids (row) = index then
               for col in Data'Range (2) loop
                  aCol (col) := Data (row, col);
               end loop;
               Cols.Append (aCol);
            end if;
         end loop;

         if Cols.Is_Empty then
            for row in Centres'Range loop
               for col in Centres'Range (2) loop
                  Centres (row, col) :=
                    Data (Maths.Random_Integer (1, Data'Length), col);
               end loop;
            end loop;
         else
            Centres := Means (Cols);
         end if;
      end loop;

      return Centres;

   end Compute_Means;

   --  ------------------------------------------------------------------------

   function Loss (Values : Real_Float_Matrix; Min_Indices : out Integer_Array)
                  return Float is
      Min_Values   : Real_Float_Vector (Values'Range (2)) :=
                       (others => Float'Safe_Large);
      Result       : Float := 0.0;
   begin
      for row in Values'Range loop
         for col in Values'Range (2) loop
            if Values (row, col) < Min_Values (col) then
               Min_Values (col) := Values (row, col);
               Min_Indices (col) := row;
            end if;
         end loop;
      end loop;

      for index in Min_Values'Range loop
         Result := Result + Min_Values (index);
      end loop;

      return Result;

   end Loss;

   --  -------------------------------------------------------------------------

   function Means (Data : Float_Array_List) return Real_Float_Matrix is
      Result : Real_Float_Matrix (1 .. Integer (Data.Length),
                                  Data.Element (1)'Range);
   begin

      return Result;

   end Means;

   --  -------------------------------------------------------------------------

end Support_11A;
