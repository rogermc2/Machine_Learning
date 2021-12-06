
with Ada.Assertions; use Ada.Assertions;
with Ada.Containers;

package body Classification_Metrics is

    function Weighted_Sum
      (Sample_Score  : Classifier_Types.Float_List;
       Sample_Weight : Weights.Weight_List :=
         Classifier_Types.Float_Package.Empty_Vector;
       Normalize     : Boolean := False) return float;

    --  ------------------------------------------------------------------------

    function Check_Targets
      (Y_True, Y_Prediction : ML_Types.Value_Data_Lists_2D)
       return Boolean is
        use Ada.Containers;
        Routine_Name : constant String := "Classification_Metrics.Check_Targets";

    begin
        Assert (Y_True.Length = Y_Prediction.Length, Routine_Name &
                  "");
        return True;

    end Check_Targets;

    --  ------------------------------------------------------------------------

    function Accuracy_Score
      (Y_True, Y_Prediction : ML_Types.Value_Data_Lists_2D;
       Normalize            : Boolean := True;
       Sample_Weight        : Weights.Weight_List :=
         Classifier_Types.Float_Package.Empty_Vector)
       return float is
        use Ada.Containers;
        use ML_Types.Value_Lists_Data_Package;
        Routine_Name : constant String := "Classification_Metrics.Accuracy_Score";
        Score : Classifier_Types.Float_List;
    begin
        Assert (Check_Targets (Y_True, Y_Prediction), Routine_Name &
                  "");
        Assert (Y_True.Length = Sample_Weight.Length, Routine_Name &
                  "");
        --          Score :=  Y_True - Y_Prediction;
        for index in Y_True.First_Index .. Y_True.Last_Index loop
            Score.Append_Vector (Y_True (index));
        end loop;
        return Weighted_Sum (Y_True, Sample_Weight, Normalize);

    end Accuracy_Score;

    --  ------------------------------------------------------------------------

    function Weighted_Sum
      (Sample_Score  : Classifier_Types.Float_List;
       Sample_Weight : Weights.Weight_List :=
         Classifier_Types.Float_Package.Empty_Vector;
       Normalize     : Boolean := False) return float is
    --        Routine_Name : constant String := "Classification_Metrics.Weighted_Sum";
        Weights_Sum : Float := 0.0;
        Result      : Float := 0.0;

        function Dot_Product return float is
            DP : Float := 0.0;
        begin
            for index in Sample_Score.First_Index ..
              Sample_Score.Last_Index loop
                DP := DP + Sample_Score.Element (index) *
                  Sample_Weight.Element (index);
            end loop;
            return DP;
        end Dot_Product;

    begin
        if Normalize then
            for index in Sample_Weight.First_Index .. Sample_Weight.Last_Index loop
                Weights_Sum := Weights_Sum + Sample_Weight.Element (index);
            end loop;
            Result := Dot_Product / Weights_Sum;

        elsif not Sample_Weight.Is_Empty then
            Result := Dot_Product;
        else
            for index in Sample_Score.First_Index .. Sample_Score.Last_Index loop
                Result := Result + Sample_Score.Element (index);
            end loop;
        end if;

        return Result;

    end Weighted_Sum;

    --  ------------------------------------------------------------------------

end Classification_Metrics;
