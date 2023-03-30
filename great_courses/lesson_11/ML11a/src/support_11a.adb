
--  with Ada.Text_IO; use Ada.Text_IO;

with Maths;

--  with Basic_Printing; use  Basic_Printing;
with ML_Types;

package body Support_11A is

   function Cluster_Mode (A : ML_Types.Integer_List) return Integer;
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
      Min_Indices  : Integer_Array (Min_Vals'Range) := (others => 0);
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
   --  Assign_Data assigns each datapoint in data to the closest cluster center.
   function Assign_Data (Data, Centres : Real_Float_Matrix;
                         Centre_Ids    : out Integer_Array) return Float is
      --        Routine_Name : constant String := "Support_11A.Assign_Data ";
      Res_Array    : Real_Float_Vector (Data'Range);
      Res2_Diffs   : Real_Float_Matrix (Centres'Range, Data'Range);
      Min_Vals     : Real_Float_Vector (Centre_Ids'Range);
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
   --  Cluster_Means categorizes the data based on their appearance.
   --  This optimizer starts with center locations and assignments of data to
   --  the centers.
   --  It then alternates between two steps.
   --  Both steps are simple are guaranteed to decrease the loss whenever they
   --  produce any change at all.
   --  The first step, Assign_Data, reassigns the data points to centers.
   --  Specifically, each data point is assigned to its closest center.
   --  The second step, Compute_Means, recomputes the centers for each cluster.
   --  Specifically, each center is moved to the mean position of the
   --  data points that it is assigned to.
   --  Either the center is already assigned to the mean of its associated
   --  data points, in which case its at a local minimum of the loss function
   --  or some center is moved.
   --  Such a move decreases the loss the most over all center relocations.
   function Cluster_Means
     (Data : Real_Float_Matrix; K : Positive; Curr_Loss : out Float;
      Test : Boolean := False) return Real_Float_Matrix is
      --        Routine_Name: constant String := "Support_11A.Cluster_Means ";
      Centres     : Real_Float_Matrix (1 .. K, Data'Range (2)) :=
                      (others => (others => 0.0));
      Centre_Ids  : Integer_Array (Data'Range);
      Prev_Loss   : Float := 0.0;
   begin
      --  kmeans
      --  initialize  k cluster centers by selecting random data points.
      Initialize_Centres (Data, K, Centres, Test);

      Curr_Loss := 1.0;
      while Prev_Loss /= Curr_Loss loop
         Prev_Loss := Curr_Loss;
         Curr_Loss := Assign_Data (Data, Centres, Centre_Ids);
         Centres := Compute_Means (Data, Centre_Ids, K, Test);
      end loop;

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
         --  Cols is an array of all points having the current center id.
         Cols.Clear;
         for index in Centre_Ids'Range loop            --  j
            if Centre_Ids (index) = cluster then
               --  Get data for Data row (cluster)
               Cols.Append (Get_Row (Data, index));
            end if;
         end loop;

         if Cols.Is_Empty then
            --  Cols is empty, which means that the cluster center is out of
            --  the action and we should pick a different location for it.
            if Test then
               for row in Centres'Range loop
                  for col in Centres'Range (2) loop
                     Centres (cluster, col) := Data (row, col);
                  end loop;
               end loop;
            else
               --  choose one of the data points at random to be the new
               --  location of this cluster centre.
               for row in Centres'Range loop
                  for col in Centres'Range (2) loop
                     Centres (cluster, col) :=
                       Data (Maths.Random_Integer (1, Data'Length), col);
                  end loop;
               end loop;
            end if;
         else
            --  Cols is not empty meaning that there are data points close to
            --  the cluster center so move that center to the mean position of the
            --  closest points.
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

   function Cluster_Mode (A : ML_Types.Integer_List) return Integer is
      --        Routine_Name : constant String := "Support_11A.Cluster_Mode ";
      Min         : Integer := Integer'Last;
      Max         : Integer := Integer'First;
      Int_Range   : Integer;
      Mode_Offset : Natural := 0;
   begin
      for index in 1 .. Integer (A.Length) loop
         if A (index) < Min then
            Min := A (index);
         end if;

         if A (index) > Max then
            Max := A (index);
         end if;
      end loop;
      --        Put_Line (Routine_Name & "Max, Min: " & Integer'Image (Max) & " " &
      --                    Integer'Image (Min));

      Int_Range := Max - Min + 1;
      declare
         Counters    : Integer_Array (0 .. Int_Range - 1) := (others => 0);
         Offset      : Natural;
      begin
         for index in 1 .. Integer (A.Length) loop
            Offset := A (index) - Min;
            Counters (Offset) := Counters (Offset) + 1;
         end loop;

         for Index in 1 .. Int_Range - 1 loop
            if Counters (Index) > Counters (Mode_Offset) then
               Mode_Offset := Index;
            end if;
         end loop;
      end;

      return Min + Mode_Offset;

   end Cluster_Mode;

   --  -------------------------------------------------------------------------

   function Compute_Ans
     (Labels         : Integer_Matrix; Center_IDs : Integer_Array;
      Cluster_Labels : Integer_Array; Num_Clusters : Positive)
      return Real_Float_List is
      Ans         : Real_Float_List;
      Labels_List : ML_Types.Integer_List;
      Sum         : Integer;
   begin
      for cluster in 1 .. Num_Clusters loop
         Labels_List.Clear;
         for lab_index in Center_IDs'Range loop
            if Center_IDs (lab_index) = Labels (cluster, 1) then
               Labels_List.Append (Cluster_Labels (cluster));
            end if;
         end loop;

         if not Labels_List.Is_Empty then
            Sum := 0;
            for index in Labels_List.First_Index .. Labels_List.Last_Index loop
               Sum := Sum + Labels_List (index);
            end loop;
            Ans.Append (Float (Sum) / Float (Labels'Length));
         end if;
      end loop;

      return Ans;

   end Compute_Ans;

   --  -------------------------------------------------------------------------

   function Compute_Cluster_Labels
     (Labels       : Integer_Matrix; Center_IDs : Integer_Array;
      Num_Clusters : Positive) return Integer_Array is
      C_Labels    : Integer_Array (1 .. Num_Clusters);
      Labels_List : ML_Types.Integer_List;
   begin
      for cluster in 1 .. Num_Clusters loop
         C_Labels (cluster) := Labels (1, 1);
      end loop;

      for cluster in 1 .. Num_Clusters loop
         Labels_List.Clear;
         for lab_index in Center_IDs'Range loop
            if Center_IDs (lab_index) = cluster then
               Labels_List.Append (Labels (lab_index, 1));
            end if;
         end loop;

         if not Labels_List.Is_Empty then
            --  use mode of label item as cluster label
            C_Labels (cluster) := Cluster_Mode (Labels_List);
         end if;

      end loop;

      return C_Labels;

   end Compute_Cluster_Labels;

   --  -------------------------------------------------------------------------

end Support_11A;
