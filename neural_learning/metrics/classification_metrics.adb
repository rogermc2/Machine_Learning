--  Based on scikit-learn/sklearn/metrics/_classification.py

--  with Ada.Assertions; use Ada.Assertions;
--  with Ada.Text_IO; use Ada.Text_IO;

--  with Printing;

package body Classification_Metrics is

   procedure Check_Targets (Y_True, Y_Prediction : Real_Float_Matrix);
   function Weighted_Sum
     (Sample_Score   : Real_Float_Matrix;
      Sample_Weights : Float_Array;
      Normalize      : Boolean := False) return float;

   --  ------------------------------------------------------------------------
   --  L144
   function Accuracy_Score
     (Y_True, Y_Prediction : Real_Float_Matrix; Normalize : Boolean := True;
     Sample_Weight : Float_Array) return float is
--        Routine_Name : constant String :=
--                         "Classification_Metrics.Accuracy_Score, ";
      Score        : Real_Float_Matrix := Y_Prediction;
   begin
      Check_Targets (Y_True, Y_Prediction);
      for row in Score'Range loop
            for col in Score'Range (2) loop
                if Y_True (row, col) = Y_Prediction (row, col) then
                    Score (row, col) := Score (row, col) - 1.0;
                end if;
            end loop;
      end loop;

      return Weighted_Sum (Score, Sample_Weight, Normalize);

   end Accuracy_Score;

   --  ------------------------------------------------------------------------
   --  Numpy Average: avg = sum(a * weights) / sum(weights)
   function Average (Sample_Score   : Float_Matrix;
                     Sample_Weights : Float_Array)
                     return Float is
      --       Routine_Name : constant String :=
      --                        "Classification_Metrics.Average ";
      Weights     : Float_Array (Sample_Score'Range (2));
      Sum_Weights : Float := 0.0;
      Sum         : Float := 0.0;
   begin
      if Sample_Weights'Last < Sample_Weights'First then
         for index in Weights'Range loop
            Weights (index) := 1.0;
         end loop;
      else
         Weights := Sample_Weights;
      end if;

      for row in Sample_Score'Range loop
         for col in Sample_Score'Range (2) loop
            Sum := Sum + Weights (col) * Sample_Score (row, col);
            Sum_Weights := Sum_Weights + Weights (col);
         end loop;
      end loop;

      return Sum / Sum_Weights;

   end Average;

   --  ------------------------------------------------------------------------

   procedure Check_Targets (Y_True, Y_Prediction : Real_Float_Matrix) is
   begin
      NL_Arrays_And_Matrices.Check_Lengths
        ("Classification_Metrics.Check_Targets", Y_True, Y_Prediction);

   end Check_Targets;

   --  ------------------------------------------------------------------------

   function Sum (Sample_Score : Float_Matrix) return float is
      Result : Float := 0.0;
   begin
      for row in Sample_Score'Range loop
         for col in Sample_Score'Range (2) loop
            Result := Result + Sample_Score (row, col);
         end loop;
      end loop;

      return Result;

   end Sum;

   --  ------------------------------------------------------------------------

   function Sum (Sample_Score : Float_Array) return float is
      Result : Float := 0.0;
   begin
      for row in Sample_Score'Range loop
         Result := Result + Sample_Score (row);
      end loop;

      return Result;

   end Sum;

   --  ------------------------------------------------------------------------

   function Weighted_Sum
     (Sample_Score   : Real_Float_Matrix; Sample_Weights : Float_Array;
      Normalize      : Boolean := False) return Float is
      --        Routine_Name : constant String := "Classification_Metrics.Weighted_Sum ";
      W_Sum       : Float;
   begin
      if Normalize then
         W_Sum := Average (Sample_Score, Sample_Weights);
      elsif Sample_Weights'Last >= Sample_Weights'First then
         W_Sum := Sum (Dot (Sample_Score, Sample_Weights));
      else
         W_Sum := Sum (Sample_Score);
      end if;

      return W_Sum;

   end Weighted_Sum;

   --  ------------------------------------------------------------------------

end Classification_Metrics;
