
with Maths;

package body Support_11A is

   function Compute_Means
     (Data : Real_Float_Matrix; Centre_Ids : Integer_Array; K : Positive)
      return Real_Float_Matrix;
   function Loss (Values : Real_Float_Matrix; Min_Indices : out Integer_Array)
                  return Float;
   function Means (Data : Float_Array_List) return Real_Float_Vector;

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
   --  kmeans
   function Cluster_Means (Data      : Real_Float_Matrix; K : Positive;
                           Curr_Loss : out Float) return Real_Float_Matrix is
      Centres    : Real_Float_Matrix (1 .. K, Data'Range (2)) :=
                     (others => (others => 0.0));
      Centre_Ids : Integer_Array (Centres'Range);
      Prev_Loss  : Float := 0.0;
   begin
      Curr_Loss := 1.0;
      for cluster in 1 .. K loop
         for col in Centres'Range (2) loop
            Centres (cluster, col) :=
              Data (Maths.Random_Integer (1, Data'Length), col);
         end loop;
      end loop;

      while Prev_Loss /= Curr_Loss loop
         Prev_Loss := Curr_Loss;
         Curr_Loss := Assign_Data (Data, Centres, Centre_Ids);
         Centres := Compute_Means (Data, Centre_Ids, K);
      end loop;

      return Centres;

   end Cluster_Means;

   --  ------------------------------------------------------------------------

   function Compute_Means (Data       : Real_Float_Matrix;
                           Centre_Ids : Integer_Array; K : Positive)
                           return Real_Float_Matrix is
      Centres    : Real_Float_Matrix (1 .. K, Data'Range (2)) :=
                     (others => (others => 0.0));
      aCol       : Float_Array (Data'Range (2));
      Cols       : Float_Array_List;  --  data points assigned to a cluster
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
                  Centres (index, col) :=
                    Data (Maths.Random_Integer (1, Data'Length), col);
               end loop;
            end loop;
         else
            declare
               Mean_Values : constant Real_Float_Vector := Means (Cols);
            begin
               for row in Centres'Range loop
                  for col in Centres'Range (2) loop
                     Centres (index, col) := Mean_Values (col);
                  end loop;
               end loop;
            end;
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

   function Means (Data : Float_Array_List) return Real_Float_Vector is
      use Real_Float_Arrays;
      aRow   : Float_Array (Data.Element (1)'Range);
      Result : Real_Float_Vector (Data.Element (1)'Range) := (others => 0.0);
   begin
      for row in Data.First_Index .. Data.Last_Index loop
         aRow := Data.Element (row);
         for col in aRow'Range loop
            Result (col) := Result (col) + aRow (col);
         end loop;
      end loop;

      return Result / Float (Data.Length);

   end Means;

   --  -------------------------------------------------------------------------

end Support_11A;
