
with Ada.Text_IO; use Ada.Text_IO;

with Maths;

with Basic_Printing; use  Basic_Printing;

package body Support_11A is

   function Compute_Diff_Vector
     (Data, Centres : Real_Float_Matrix; Centre_Row : Positive)
      return Real_Float_Matrix;
   function Compute_Means
     (Data : Real_Float_Matrix; Centre_Ids : Integer_Array; K : Positive)
      return Real_Float_Matrix;
   function Loss (Values : Real_Float_Vector) return Float;
   function Means (Data : Float_Array_List) return Real_Float_Vector;

   --  ------------------------------------------------------------------------

   function Arg_Min (Values   : Real_Float_Matrix;
                     Min_Vals : out Real_Float_Vector) return Integer_Array is
      Min_Indices  : Integer_Array (Values'Range);
   begin
      for row in Values'Range loop
         Min_Vals (row) := Values (row, 1);
         Min_Indices (row) := 1;
         for col in 2 .. Values'Length (2) loop
            if Values (row, col) < Min_Vals (row) then
               Min_Vals (row) := Values (row, col);
               Min_Indices (row) := col;
            end if;
         end loop;
      end loop;

      return Min_Indices;

   end Arg_Min;

   --  -------------------------------------------------------------------------
   --  Assign_Data takes the data and the centers for each cluster and
   --  assigns each datapoint in data to the closest of the centers, centerids.
   function Assign_Data (Data, Centres : Real_Float_Matrix;
                         Centre_Ids    : out Integer_Array) return Float is
      Routine_Name : constant String := "Support_11A.Assign_Data ";
      Values       : Real_Float_Matrix (Data'Range, Data'Range (2));
--        Centre_Diffs : Integer_Array (Data'Range);
      Min_Vals     : Real_Float_Vector (Data'Range);
      Result       : Float := 0.0;
   begin
      Put_Line (Routine_Name);
      --  subtract the set of centers from each data point
      --  For each centre, Compute_Diff_Vector finds the difference between each
      --  value and the corresponding data value
      for row in Centres'Range loop
         Values := Compute_Diff_Vector (Data, Centres, row);
         for col in Centres'Range (2) loop
            Centre_Ids := Arg_Min (Values, Min_Vals);
            if Min_Vals (col) > 0.0 then
               Put_Line (Routine_Name & "Min_Vals > 0.0" & Integer'Image (col));
            end if;
         end loop;
      end loop;
--        Print_Float_Matrix (Routine_Name & "Centre_Diffs", Centre_Diffs,
--                            1, 4, 130, 140);

      --  assign each data point to its closest center
--        Centre_Ids := Arg_Min (Centre_Diffs, Values);
      Result := Loss (Min_Vals);
      Put_Line (Routine_Name & "Result" & Float'Image (Result));
      return Result;

   end Assign_Data;

   --  ------------------------------------------------------------------------

   function Compute_Diff_Vector
     (Data, Centres : Real_Float_Matrix; Centre_Row : Positive)
      return Real_Float_Matrix is
      Routine_Name: constant String := "Support_11A.Compute_Diff_Vector ";
      Diffs    : Real_Float_Matrix (Data'Range, Data'Range (2));
   begin
      Print_Float_Matrix (Routine_Name & "Data", Data, 1, 1, 130, 140);
      Print_Float_Matrix (Routine_Name & "Centres", Centres, 1, 1, 130, 140);
      --  subtract the set of centers from each data point
      for d_row in Data'Range loop
         for col in Data'Range (2) loop
            Diffs (d_row, col) := (Data (d_row, col) - Centres (Centre_Row, col)) ** 2;
         end loop;
      end loop;

      return Diffs;

   end Compute_Diff_Vector;

   --  ------------------------------------------------------------------------
   --  kmeans
   function Cluster_Means (Data      : Real_Float_Matrix; K : Positive;
                           Curr_Loss : out Float) return Real_Float_Matrix is
      Routine_Name: constant String := "Support_11A.Cluster_Means ";
      Centres     : Real_Float_Matrix (1 .. K, Data'Range (2)) :=
        (others => (others => 0.0));
      Centre_Ids  : Integer_Array (Centres'Range);
      Prev_Loss   : Float := 0.0;
   begin
      --        Print_Float_Matrix (Routine_Name & "Data", Data, 21, 22, 130, 140);
      for cluster in 1 .. K loop
         for col in Centres'Range (2) loop
            Centres (cluster, col) :=
              Data (Maths.Random_Integer (1, Data'Length), col);
         end loop;
      end loop;
      Print_Matrix_Dimensions (Routine_Name & "Centres", Centres);
      --        Print_Float_Matrix (Routine_Name & "Centres", Centres,
      --                            1, 3, 120, 140);

      Curr_Loss := 1.0;
      while Prev_Loss /= Curr_Loss loop
         Prev_Loss := Curr_Loss;
         Put_Line (Routine_Name & "Prev_Loss" & Float'Image (Prev_Loss));
         Curr_Loss := Assign_Data (Data, Centres, Centre_Ids);
         Put_Line (Routine_Name & "Curr_Loss" & Float'Image (Curr_Loss));
         Centres := Compute_Means (Data, Centre_Ids, K);
      end loop;

      return Centres;

   end Cluster_Means;

   --  ------------------------------------------------------------------------

   function Compute_Means (Data       : Real_Float_Matrix;
                           Centre_Ids : Integer_Array; K : Positive)
                           return Real_Float_Matrix is
      Routine_Name: constant String := "Support_11A.Compute_Means ";
      Centres     : Real_Float_Matrix (1 .. K, Data'Range (2)) :=
        (others => (others => 0.0));
      aCol        : Float_Array (Data'Range (2));
      Cols        : Float_Array_List;  --  data points assigned to a cluster
   begin
      Put_Line (Routine_Name);
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
         Put_Line (Routine_Name & "Cols loaded");
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

   function Loss (Values : Real_Float_Vector) return Float is
      Result : Float := 0.0;
   begin

      for index in Values'Range loop
         Result := Result + Values (index);
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
