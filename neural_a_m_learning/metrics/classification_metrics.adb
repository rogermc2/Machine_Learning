--  Based on scikit-learn/sklearn/metrics/_classification.py

--  with Ada.Assertions; use Ada.Assertions;
--  with Ada.Text_IO; use Ada.Text_IO;

package body Classification_Metrics is

   function Weighted_Sum
     (Sample_Score   : Float_Matrix;
      Sample_Weights : Float_Array;
      Normalize      : Boolean := False) return float;

   --  ------------------------------------------------------------------------

   function Accuracy_Score
     (Y_True, Y_Prediction : Float_Matrix;
      Normalize            : Boolean := True;
      Sample_Weight        : Float_Array)
      return float is
      --        Routine_Name : constant String :=
      --                         "Classification_Metrics.Accuracy_Score, ";
      Result : constant Float_Matrix := Y_Prediction - Y_True;
   begin

      return Weighted_Sum (Result, Sample_Weight, Normalize);

   end Accuracy_Score;

   --  ------------------------------------------------------------------------
   --  Numpy Average: avg = sum(a * weights) / sum(weights)
   function Average (Sample_Score   : Float_Matrix;
                     Sample_Weights : Float_Array)
                     return Float is
      Weights     : Float_Array (Sample_Score'Range (2));
      Sum_Weights : Float := 0.0;
      Result      : Float := 0.0;
   begin
      if Sample_Weights'Length /= Sample_Score'Length (2) then
         for index in Sample_Score'Range loop
            Weights (index) := 1.0;
         end loop;
      end if;

      for row in Sample_Score'Range loop
         for col in Sample_Score'Range (2) loop
            Result := Weights (col) * Sample_Score (row, col);
            Sum_Weights := Sum_Weights + Weights (col);
         end loop;
      end loop;

      return Result / Sum_Weights;

   end Average;

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
     (Sample_Score   : Float_Matrix; Sample_Weights : Float_Array;
      Normalize      : Boolean := False) return Float is
--        Routine_Name : constant String := "Classification_Metrics.Weighted_Sum ";
      Result       : Float;
   begin
      if Normalize then
         Result := Average (Sample_Score, Sample_Weights);
      elsif Sample_Weights'Last >= Sample_Weights'First then
         Result := Sum (Dot (Sample_Score, Sample_Weights));
      else
         Result := Sum (Sample_Score);
      end if;

      return Result;

   end Weighted_Sum;

   --  ------------------------------------------------------------------------

end Classification_Metrics;
