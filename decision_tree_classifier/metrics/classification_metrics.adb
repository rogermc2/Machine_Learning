
with Ada.Assertions; use Ada.Assertions;
with Ada.Containers;

package body Classification_Metrics is

   function Weighted_Sum
     (Sample_Score  : Classifier_Types.Float_List;
      Sample_Weight : Weights.Weight_List :=
        Classifier_Types.Float_Package.Empty_Vector;
      Normalize     : Boolean := False) return float;

   --  ------------------------------------------------------------------------

   function Accuracy_Score
     (Actual, Prediction : ML_Types.Value_Data_Lists_2D;
      Normalize          : Boolean := True;
      Sample_Weight      : Weights.Weight_List :=
        Classifier_Types.Float_Package.Empty_Vector)
      return float is
      use Ada.Containers;
      Routine_Name : constant String := "Classification_Metrics.Accuracy_Score";
   begin
      Assert (Actual.Length = Prediction.Length, Routine_Name &
                "");
      Assert (Actual.Length = Sample_Weight.Length, Routine_Name &
                "");
      return 0.0;

   end Accuracy_Score;

   --  ------------------------------------------------------------------------

   function Weighted_Sum
     (Sample_Score  : Classifier_Types.Float_List;
      Sample_Weight : Weights.Weight_List :=
        Classifier_Types.Float_Package.Empty_Vector;
      Normalize     : Boolean := False) return float is
      --        Routine_Name : constant String := "Classification_Metrics.Weighted_Sum";
      Result       : Float := 0.0;
   begin
      if Normalize then
         null;
--           Result := Average (Sample_Score, Sample_Weight);
      else
         for index in Sample_Score.First_Index .. Sample_Score.Last_Index loop
            Result := Result + Sample_Score.Element (index);
         end loop;
      end if;

      return Result;

   end Weighted_Sum;

   --  ------------------------------------------------------------------------

end Classification_Metrics;
