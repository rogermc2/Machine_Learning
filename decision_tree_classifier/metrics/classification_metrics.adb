
with Ada.Assertions; use Ada.Assertions;
with Ada.Containers;

package body Classification_Metrics is

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

end Classification_Metrics;
