
with Ada.Text_IO; use Ada.Text_IO;

with Maths;

with Basic_Printing; use  Basic_Printing;

package body Support_11A is

   function Compute_Diff_Vector (Data, Centres : Real_Float_Matrix)
                                 return Real_Float_Vector;
   function Compute_Means
     (Data : Real_Float_Matrix; Centre_Ids : Integer_Array; K : Positive)
      return Real_Float_Matrix;
   function Loss (Values : Real_Float_Matrix; Min_Indices : out Integer_Array)
                  return Float;
   function Means (Data : Float_Array_List) return Real_Float_Vector;

   --  ------------------------------------------------------------------------

   function Assign_Data (Data, Centres : Real_Float_Matrix;
                         Centre_Ids    : out Integer_Array) return Float is
      Routine_Name : constant String := "Support_11A.Assign_Data ";
      Diffs        : Real_Float_Vector (Data'Range);
      Centre_Diffs : Real_Float_Matrix (Centres'Range, Data'Range);
      Result       : Float := 0.0;
   begin
      Put_Line (Routine_Name);
      --  subtract the set of centers from each data point
      for row in Centre_Diffs'Range loop
         Diffs := Compute_Diff_Vector (Data, Centres);
         for col in Centre_Diffs'Range (2) loop
            Centre_Diffs (row, col) := Diffs (col);
         end loop;
      end loop;

      --  sum the squared differences
      --  res2 = np.add.reduce(res**2,2)
      --        for d3 in Centres'Range loop
      --           for row in Data'Range loop
      --              for col in Data'Range (2) loop
      --                 Diff (col) := Data (row, col) - Centres (d3, col);
      --                 Diff (col) := Diff (col) ** 2;
      --                 Centres (d3, col) := Diff (col);
      --              end loop;
      --           end loop;
      --        end loop;

      --        for d3 in Diff2'Range loop
      --           for row in Total'Range loop
      --              for col in Total'Range (2) loop
      --                 Total (row, col) := Total (row, col) + Diff2  (d3, row, col);
      --              end loop;
      --           end loop;
      --        end loop;

      --        Result := Loss (Total, Centre_Ids);
      Put_Line (Routine_Name & "Result" & Float'Image (Result));
      return Result;

   end Assign_Data;

   --  ------------------------------------------------------------------------

   function Compute_Diff_Vector (Data, Centres : Real_Float_Matrix)
                                 return Real_Float_Vector is
      C_Length : constant Positive := Centres'Length;
      Diffs    : Real_Float_Vector (Data'Range);
   begin
      --  subtract the set of centers from each data point
      for d_row in Data'Range loop
         for c_row in 1 .. C_Length loop
            for col in Data'Range (2) loop
               Diffs (d_row) := (Data (d_row, col) - Centres (c_row, col)) ** 2;
            end loop;
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
      Put_Line (Routine_Name);
      for cluster in 1 .. K loop
         for col in Centres'Range (2) loop
            Centres (cluster, col) :=
              Data (Maths.Random_Integer (1, Data'Length), col);
         end loop;
      end loop;
      Put_Line (Routine_Name & "Centres loaded");
      Print_Matrix_Dimensions (Routine_Name & "Centres", Centres);

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
