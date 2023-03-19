
with Ada.Text_IO; use Ada.Text_IO;

with Maths;

with Basic_Printing; use  Basic_Printing;

package body Support_11A is

   function Compute_Means
     (Data : Real_Float_Matrix; Centre_Ids : Integer_Array; K : Positive;
      Test : Boolean := False) return Real_Float_Matrix;
   function Means (Data : Float_Array_List) return Real_Float_Vector;

   --  ------------------------------------------------------------------------
   --  Add_Reduce_Differences (Data, Centres, n) finds the difference  between
   --  each data value (m, p) and the corresponding centre value (n, p)
   --  Res_N (m, p) is Data (m, p) - Centres (n, p)
   function Add_Reduce_Differences
     (Data, Centres : Real_Float_Matrix; Centre_Row : Positive)
      return Real_Float_Vector is
      --        Routine_Name: constant String := "Support_11A.Add_Reduce_Differences ";
      Res_N       : Real_Float_Matrix (Data'Range, Data'Range (2));
      Result      : Real_Float_Vector (Data'Range);
   begin
      --        Print_Float_Matrix (Routine_Name & "Data", Data, 1, 1, 130, 140);
      --        Print_Float_Matrix (Routine_Name & "Centres", Centres, 1, 1, 130, 140);
      --  subtract the set of centers from each data point
      for m in Res_N'Range loop
         for p in Res_N'Range (2) loop
            Res_N (m, p) := (Data (m, p) - Centres (Centre_Row, p)) ** 2;
         end loop;
      end loop;

      --  add.reduce(res**2,2)
      for row in Result'Range loop
         Result (1) := Res_N (row, 1);
         for col in 2 .. Res_N'Length (2) loop
            Result (row) := Result (row) + Res_N (row, col);
         end loop;
      end loop;

      return Result;

   end Add_Reduce_Differences;

   --  ------------------------------------------------------------------------

   function Arg_Min (Res2     : Real_Float_Matrix;
                     Min_Vals : out Real_Float_Vector) return Integer_Array is
      --        Routine_Name : constant String := "Support_11A.Arg_Min ";
      Min_Indices  : Integer_Array (Res2'Range (2)) := (others => 0);
   begin
      for index in Min_Vals'Range loop
         Min_Vals (index) := Float'Safe_Last;
      end loop;

      --        Print_Matrix_Dimensions (Routine_Name & "Res2", Res2);
      for row in Res2'Range loop
         for col in Res2'Range (2) loop
            if Res2 (row, col) < Min_Vals (col) then
               Min_Vals (col) := Res2 (row, col);
               Min_Indices (col) := col;
            end if;
         end loop;
      end loop;

      --        Print_Integer_Array (Routine_Name & "Min_Indices 1 - 8",
      --                             Min_Indices, 1, 8);
      return Min_Indices;

   end Arg_Min;

   --  -------------------------------------------------------------------------
   --  Assign_Data takes the data and the centers for each cluster and
   --  assigns each datapoint in data to the closest of the centers, centerids.
   function Assign_Data (Data, Centres : Real_Float_Matrix;
                         Centre_Ids    : out Integer_Array) return Float is
      Routine_Name : constant String := "Support_11A.Assign_Data ";
      Res_Array    : Real_Float_Vector (Data'Range);
      Res2_Diffs   : Real_Float_Matrix (Centres'Range, Data'Range);
      Min_Vals     : Real_Float_Vector (Data'Range);
      Result       : Float := 0.0;
   begin
      --  subtract the set of centers from each data point
      --  For each centre c (n), Add_Reduce_Differences finds the difference
      --  between each data value data (m, p) and the corresponding centre
      --  value centre (n, p), squares each value then adds the values of each
      --  row together.
      --  res (n, m) is data (m) - centre (n)
      --  res_n (m, p) is Data (m, p) - Centres (n, p)
      Print_Float_Matrix (Routine_Name & "Centres", Centres, 1, 3, 7, 10);
      for row in Res2_Diffs'Range loop
         Res_Array := Add_Reduce_Differences (Data, Centres, row);
         for col in Res2_Diffs'Range (2) loop
            Res2_Diffs (row, col) := Res_Array (col);
         end loop;
      end loop;
      Print_Float_Matrix (Routine_Name & "Res2_Diffs", Res2_Diffs,
                          1, 3, 7, 10);

      --  assign each data point to its closest center
      Centre_Ids := Arg_Min (Res2_Diffs, Min_Vals);
      Print_Float_Vector (Routine_Name & "Min_Vals", Min_Vals, 7, 10);

      for index in Min_Vals'Range loop
         Result := Result + Min_Vals (index);
      end loop;
      Put_Line (Routine_Name & "Loss: " & Float'Image (Result));
      return Result;

   end Assign_Data;

   --  ------------------------------------------------------------------------
   --  kmeans
   function Cluster_Means
     (Data : Real_Float_Matrix; K : Positive; Curr_Loss : out Float;
      Test : Boolean := False) return Real_Float_Matrix is
      Routine_Name: constant String := "Support_11A.Cluster_Means ";
      Centres     : Real_Float_Matrix (1 .. K, Data'Range (2)) :=
                      (others => (others => 0.0));
      Centre_Ids  : Integer_Array (Data'Range);
      Prev_Loss   : Float := 0.0;
      Count       : Natural := 0;
   begin
      for cluster in 1 .. K loop
         for col in Centres'Range (2) loop
            Centres (cluster, col) :=
              Data (Maths.Random_Integer (1, Data'Length), col);
         end loop;
      end loop;

      Curr_Loss := 1.0;
      while Prev_Loss /= Curr_Loss and Count < 4 loop
         Count := Count + 1;
         Prev_Loss := Curr_Loss;
         Curr_Loss := Assign_Data (Data, Centres, Centre_Ids);
         Put_Line (Routine_Name & "Curr_Loss: " & Float'Image (Curr_Loss));
         Centres := Compute_Means (Data, Centre_Ids, K, Test);
      end loop;
      New_Line;

      return Centres;

   end Cluster_Means;

   --  ------------------------------------------------------------------------

   function Compute_Means
     (Data : Real_Float_Matrix; Centre_Ids : Integer_Array; K : Positive;
      Test : Boolean := False) return Real_Float_Matrix is
      --        Routine_Name : constant String := "Support_11A.Compute_Means ";
      Centres      : Real_Float_Matrix (1 .. K, Data'Range (2)) :=
                       (others => (others => 0.0));
      aCol         : Float_Array (Data'Range (2));
      Cols         : Float_Array_List;  --  data points assigned to a cluster
   begin
      for cluster in 1 .. K loop               --  i
         Cols.Clear;
         for row in Data'Range loop            --  j
            if Centre_Ids (row) = cluster then
               --  Get data for Data row (cluster)
               for col in Data'Range (2) loop
                  aCol (col) := Data (row, col);
               end loop;
               Cols.Append (aCol);
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
         else
            declare
               Mean_Values : constant Real_Float_Vector := Means (Cols);
            begin
               for row in Centres'Range loop
                  for col in Centres'Range (2) loop
                     Centres (cluster, col) := Mean_Values (col);
                  end loop;
               end loop;
            end;
         end if;
      end loop;

      return Centres;

   end Compute_Means;

   --  ------------------------------------------------------------------------

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
