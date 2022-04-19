--  Based on scikit-learn/sklearn/metrics/_classification.py

with Ada.Assertions; use Ada.Assertions;
--  with Ada.Text_IO; use Ada.Text_IO;

package body Classification_Metrics is

   function Weighted_Sum
     (Sample_Score   : Integer_Matrix;
      Sample_Weights : NL_Types.Float_List :=
        NL_Types.Float_Package.Empty_Vector;
      Normalize      : Boolean := False) return float;

   --  ------------------------------------------------------------------------

   function Accuracy_Score
     (Y_True, Y_Prediction : Integer_Matrix;
      Normalize            : Boolean := True;
      Sample_Weight        : NL_Types.Float_List :=
        NL_Types.Float_Package.Empty_Vector)
       return float is
--        Routine_Name : constant String :=
--                         "Classification_Metrics.Accuracy_Score, ";
      Result : constant Integer_Matrix := Y_Prediction - Y_True;
   begin

      return Weighted_Sum (Result, Sample_Weight, Normalize);

   end Accuracy_Score;

   --  ------------------------------------------------------------------------
   --  Numpy Average: avg = sum(a * weights) / sum(weights)
   function Average (Sample_Score   : Integer_Matrix;
                     Sample_Weights : NL_Types.Float_List :=
                       NL_Types.Float_Package.Empty_Vector)
                      return Float is
      use NL_Types;
      Weights     : Float_List := Sample_Weights;
      Sum_Weights : Float := 0.0;
      Result      : Float := 0.0;
   begin
      if Weights.Is_Empty then
         for index in Sample_Score'Range loop
            Weights.Append (1.0);
         end loop;
      end if;

      for row in Sample_Score'Range loop
         for col in Sample_Score'Range (2) loop
            Result := Weights.Element (col) * Float (Sample_Score (row, col));
            Sum_Weights := Sum_Weights + Weights.Element (col);
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

   function Weighted_Sum
     (Sample_Score   : Integer_Matrix;
      Sample_Weights : NL_Types.Float_List :=
        NL_Types.Float_Package.Empty_Vector;
      Normalize      : Boolean := False) return Float is
      Routine_Name : constant String := "Classification_Metrics.Weighted_Sum ";
      Result       : Float := 0.0;
   begin
      if Normalize then
         Result := Average (Sample_Score, Sample_Weights);
      elsif not Sample_Weights.Is_Empty then
         Result := Dot (Sample_Weights, To_Float_Matrix (Sample_Score));
      else
         Result := Sum (To_Float_Matrix (Sample_Score));
      end if;

      return Result;

   end Weighted_Sum;

   --  ------------------------------------------------------------------------

end Classification_Metrics;
