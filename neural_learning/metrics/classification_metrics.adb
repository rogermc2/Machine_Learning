--  Based on scikit-learn/sklearn/metrics/_classification.py

with Ada.Assertions; use Ada.Assertions;
with Ada.Text_IO; use Ada.Text_IO;

with Multiclass_Utils;
with Neural_Maths;
with Printing;

package body Classification_Metrics is

   procedure Check_Targets (Y_True, Y_Prediction : Real_Float_Matrix);
   function Weighted_Sum
     (Sample_Score   : Real_Float_Matrix;
      Sample_Weights : Real_Float_Vector;
      Normalize      : Boolean := False) return float;

   --  ------------------------------------------------------------------------
   --  L144
   function Accuracy_Score
     (Y_True, Y_Prediction : Real_Float_Matrix; Normalize : Boolean := True;
      Sample_Weight        : Real_Float_Vector) return float is
      Routine_Name : constant String :=
                        "Classification_Metrics.Accuracy_Score, ";
      eps          : constant Float := 10.0 ** (-8);
      Score        : Real_Float_Matrix (Y_True'Range, Y_True'Range (2)) :=
                       (others => (others => 1.0));
   begin
      Check_Targets (Y_True, Y_Prediction);
--        Printing.Print_Float_Matrix (Routine_Name & "Y_True", Y_True, 1, 2);
--        Printing.Print_Float_Matrix (Routine_Name & "Y_Prediction",
--                                     Y_Prediction, 1, 2);
      for row in Score'Range loop
         for col in Score'Range (2) loop
            if abs (Y_Prediction (row, col) - Y_True (row, col)) > eps then
               Score (row, col) := 0.0;
            end if;
         end loop;
      end loop;
      Printing.Print_Float_Matrix (Routine_Name & "Score", Score, 1, 2);

      return Weighted_Sum (Score, Sample_Weight, Normalize);

   end Accuracy_Score;

   --  ------------------------------------------------------------------------
   --  Numpy Average: avg = sum(a * weights) / sum(weights)
   --  if weights is not defined, avg = mean of a
   function Average (Sample_Score   : Real_Float_Matrix;
                     Sample_Weights : Real_Float_Vector)
                     return Float is
      --       Routine_Name : constant String :=
      --                        "Classification_Metrics.Average ";
      Sum_Weights : Float := 0.0;
      Sum         : Float := 0.0;
      Result      : Float := 0.0;
   begin
      if Sample_Weights'Last < Sample_Weights'First then
         --  Sample_Weights is not defined
         Result := Neural_Maths.Mean (Sample_Score);
      else
         for row in Sample_Score'Range loop
            for col in Sample_Score'Range (2) loop
               Sum := Sum + Sample_Weights (col) * Sample_Score (row, col);
               Sum_Weights := Sum_Weights + Sample_Weights (col);
            end loop;
         end loop;
         Result := Sum / Sum_Weights;
      end if;

      return Result;

   end Average;

   --  ------------------------------------------------------------------------

   procedure Check_Targets (Y_True, Y_Prediction : Real_Float_Matrix) is
      use Multiclass_Utils;
      Routine_Name : constant String := "Classification_Metrics.Check_Targets";
      Type_True : constant Y_Type := Type_Of_Target (Y_True);
      Type_Pred : Y_Type := Type_Of_Target (Y_Prediction);
   begin
      Put_Line (Routine_Name & "True type:" & Y_Type'Image (Type_True));
      Put_Line (Routine_Name & "Prediction type:" & Y_Type'Image (Type_Pred));

      NL_Arrays_And_Matrices.Check_Lengths
        ("Classification_Metrics.Check_Targets", Y_True, Y_Prediction);
      if Type_True = Y_Binary and Type_Pred = Y_Multiclass then
         Type_Pred := Y_Multiclass;
      end if;

      Assert (Type_True = Y_Binary or Type_True = Y_Multiclass or
                Type_True = Y_Multilabel_Indicator, Routine_Name & "Y_Type " &
                Y_Type'Image (Type_True) & " is not supported");
      if Type_True = Y_Binary then
         null;
      end if;
   end Check_Targets;

   --  ------------------------------------------------------------------------

   function Sum (Sample_Score : Real_Float_Matrix) return Float is
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

   function Sum (Sample_Score : Real_Float_Vector) return Float is
      Result : Float := 0.0;
   begin
      for row in Sample_Score'Range loop
         Result := Result + Sample_Score (row);
      end loop;

      return Result;

   end Sum;

   --  ------------------------------------------------------------------------

   function Weighted_Sum
     (Sample_Score   : Real_Float_Matrix; Sample_Weights : Real_Float_Vector;
      Normalize      : Boolean := False) return Float is
      use Real_Float_Arrays;
      --        Routine_Name : constant String := "Classification_Metrics.Weighted_Sum ";
      W_Sum       : Float;
   begin
      if Normalize then
         W_Sum := Average (Sample_Score, Sample_Weights);
      elsif Sample_Weights'Last >= Sample_Weights'First then
         W_Sum := Sum (Sample_Score * Sample_Weights);
      else
         W_Sum := Sum (Sample_Score);
      end if;

      return W_Sum;

   end Weighted_Sum;

   --  ------------------------------------------------------------------------

end Classification_Metrics;
