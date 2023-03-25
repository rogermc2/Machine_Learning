
with Ada.Text_IO; use Ada.Text_IO;

with Maths;

--  with Basic_Printing; use  Basic_Printing;

package body Support_11A is

   function Compute_Means
     (Data : Real_Float_Matrix; Centre_Ids : Integer_Array; K : Positive;
      Test : Boolean := False) return Real_Float_Matrix;
   procedure Initialize_Centres
     (Data    : Real_Float_Matrix; Num_Clusters : Positive;
      Centres : out Real_Float_Matrix; Test : Boolean);
   function Means (Data : Float_Vector_List) return Real_Float_Vector;

   --  ------------------------------------------------------------------------
   --  Add_Reduce_Differences (Data, Centres, n) finds the difference  between
   --  each data value Data (m, p) and the corresponding centre value (n, p)
   --  Res_N (m, p) is Data (m, p) - Centres (n, p)
   function Add_Reduce_Differences
     (Data, Centres : Real_Float_Matrix; Centre_Row : Positive)
      return Real_Float_Vector is
      --  Routine_Name : constant String := "Support_11A.Add_Reduce_Differences ";
      Res_N2       : Real_Float_Matrix (Data'Range, Data'Range (2));
      Result       : Real_Float_Vector (Data'Range) := (others => 0.0);
   begin
      --  subtract the set of centers from each data point
      for m in Data'Range loop
         for p in Data'Range (2) loop
            Res_N2 (m, p) := (Data (m, p) - Centres (Centre_Row, p)) ** 2;
         end loop;
      end loop;

      --  add.reduce(res**2,2)
      for row in Res_N2'Range loop
         for col in Res_N2'Range (2) loop
            Result (row) := Result (row) + Res_N2 (row, col);
         end loop;
      end loop;

      return Result;

   end Add_Reduce_Differences;

   --  ------------------------------------------------------------------------

   function Arg_Min (Res2     : Real_Float_Matrix;
                     Min_Vals : out Real_Float_Vector) return Integer_Array is
--        Routine_Name : constant String := "Support_11A.Arg_Min ";
      Min_Indices  : Integer_Array (Res2'Range (2)) := (others => 0);
      Min_Val      : Float;
      Min_Row      : Positive;
   begin
      for col in Res2'Range (2) loop
         Min_Val := Float'Safe_Last;
         for row in Res2'Range loop
            if Res2 (row, col) < Min_Val then
               Min_Val := Res2 (row, col);
               Min_Row := row;
            end if;
         end loop;
         Min_Vals (col) := Min_Val;
         Min_Indices (col) := Min_Row;
      end loop;

      return Min_Indices;

   end Arg_Min;

   --  -------------------------------------------------------------------------
   --  Assign_Data takes the data and the centers for each cluster and
   --  assigns each datapoint in data to the closest of the centers, centerids.
   function Assign_Data (Data, Centres : Real_Float_Matrix;
                         Centre_Ids    : out Integer_Array) return Float is
--        Routine_Name : constant String := "Support_11A.Assign_Data ";
      Res_Array    : Real_Float_Vector (Data'Range);
      Res2_Diffs   : Real_Float_Matrix (Centres'Range, Data'Range);
      Min_Vals     : Real_Float_Vector (Data'Range);
      Loss         : Float := 0.0;
   begin
      --  subtract the set of centers from each data point
      --  For each centre c (n), Add_Reduce_Differences finds the difference
      --  between each data value data (m, p) and the corresponding centre
      --  value centre (n, p), squares each value then adds the values of each
      --  row together.
      --  res (n, m) is data (m) - centre (n)
      --  res_n (m, p) is Data (m, p) - Centres (n, p)
      --        Print_Float_Matrix (Routine_Name & "Centres", Centres, 1, 3);
      for row in Centres'Range loop
         Res_Array := Add_Reduce_Differences (Data, Centres, row);
         for col in Res2_Diffs'Range (2) loop
            Res2_Diffs (row, col) := Res_Array (col);
         end loop;
      end loop;

      --  assign each data point to its closest center
      Centre_Ids := Arg_Min (Res2_Diffs, Min_Vals);

      for index in Min_Vals'Range loop
         Loss := Loss + Min_Vals (index);
      end loop;

      return Loss;

   end Assign_Data;

   --  ------------------------------------------------------------------------
   --  kmeans
   function Cluster_Means
     (Data : Real_Float_Matrix; K : Positive; Curr_Loss : out Float;
      Test : Boolean := False) return Real_Float_Matrix is
--        Routine_Name: constant String := "Support_11A.Cluster_Means ";
      Centres     : Real_Float_Matrix (1 .. K, Data'Range (2)) :=
        (others => (others => 0.0));
      Centre_Ids  : Integer_Array (Data'Range);
      Prev_Loss   : Float := 0.0;
--        Count       : Natural := 0;
   begin
      --  kmeans
      Initialize_Centres (Data, K, Centres, Test);

      Curr_Loss := 1.0;
      while Prev_Loss /= Curr_Loss loop
--           Count := Count + 1;
         Prev_Loss := Curr_Loss;
         Curr_Loss := Assign_Data (Data, Centres, Centre_Ids);
         Centres := Compute_Means (Data, Centre_Ids, K, Test);
      end loop;
      New_Line;

      return Centres;

   end Cluster_Means;

   --  ------------------------------------------------------------------------

   function Compute_Means
     (Data : Real_Float_Matrix; Centre_Ids : Integer_Array; K : Positive;
      Test : Boolean := False) return Real_Float_Matrix is
      use Real_Float_Arrays;
      --        Routine_Name : constant String := "Support_11A.Compute_Means ";
      Centres      : Real_Float_Matrix (1 .. K, Data'Range (2)) :=
        (others => (others => 0.0));
      Cols         : Float_Vector_List;  --  data points assigned to a cluster
   begin
      for cluster in 1 .. K loop               --  i
         --  Gather the data points assigned to cluster i
         --  cols = np.array([data[j] for j in range(n) if centerids[j] == i])
         Cols.Clear;
         for index in Centre_Ids'Range loop            --  j
            if Centre_Ids (index) = cluster then
               --  Get data for Data row (cluster)
               Cols.Append (Get_Row (Data, index));
            end if;
         end loop;

         if Cols.Is_Empty then
            if Test then
               for row in Centres'Range loop
                  for col in Centres'Range (2) loop
                     Centres (cluster, col) := Data (row, col);
                  end loop;
               end loop;
            else
               for row in Centres'Range loop
                  for col in Centres'Range (2) loop
                     Centres (cluster, col) :=
                       Data (Maths.Random_Integer (1, Data'Length), col);
                  end loop;
               end loop;
            end if;
         else  --  Cols not Empty
            declare
               Mean_Col_Values : constant Real_Float_Vector := Means (Cols);
            begin
               for row in Centres'Range loop
                  for col in Centres'Range (2) loop
                     Centres (cluster, col) := Mean_Col_Values (col);
                  end loop;
               end loop;
            end;
         end if;
      end loop; --  for clusters

      return Centres;

   end Compute_Means;

   --  ------------------------------------------------------------------------

   procedure Initialize_Centres
     (Data    : Real_Float_Matrix; Num_Clusters : Positive;
      Centres : out Real_Float_Matrix; Test : Boolean) is
   begin
      for cluster in 1 .. Num_Clusters loop
         if Test then
            for col in Centres'Range (2) loop
               Centres (cluster, col) := Data (cluster, col);
            end loop;
         else
            for col in Centres'Range (2) loop
               Centres (cluster, col) :=
                 Data (Maths.Random_Integer (1, Data'Length), col);
            end loop;
         end if;
      end loop;

   end Initialize_Centres;

   --  ------------------------------------------------------------------------

   function Means (Data : Float_Vector_List) return Real_Float_Vector is
      use Real_Float_Arrays;
      aRow   : Real_Float_Vector (Data.Element (1)'Range);
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
