
with Ada.Assertions; use Ada.Assertions;
--  with Ada.Text_IO; use Ada.Text_IO;

package body Classification_Metrics is

   function Weighted_Sum
     (Sample_Score   : IL_Types.Value_Data_Lists_2D;
      Sample_Weights : Weights.Weight_List :=
        Classifier_Types.Float_Package.Empty_Vector;
      Normalize      : Boolean := False) return float;

   --  ------------------------------------------------------------------------

   function Accuracy_Score
     (Y_True, Y_Prediction : IL_Types.Value_Data_Lists_2D;
      Normalize            : Boolean := True;
      Sample_Weight        : Weights.Weight_List :=
        Classifier_Types.Float_Package.Empty_Vector)
       return float is
      use IL_Types;
      Routine_Name : constant String :=
                       "Classification_Metrics.Accuracy_Score, ";
      Score        : Value_Data_Lists_2D;
   begin
      Check_Lengths (Routine_Name & " True, Prediction: ",
                     Y_True, Y_Prediction);
      if not Sample_Weight.Is_Empty then
         Classifier_Types.Check_Length
           (Routine_Name & " True, Sample_Weight: ", Sample_Weight, Y_True);
      end if;

      Score := Y_Prediction = Y_True;

      return Weighted_Sum (Score, Sample_Weight, Normalize);

   end Accuracy_Score;

   --  ------------------------------------------------------------------------

   function Average (Weight     : Classifier_Types.Float_List;
                     Boolean_2D : IL_Types.Value_Data_Lists_2D)
                      return Float is
      use IL_Types;
      Weights     : Classifier_Types.Float_List := Weight;
      Values      : Value_Data_List;
      Value       : Value_Record;
      Sum_Weights : Float := 0.0;
      Result      : Float := 0.0;
   begin
      if Weights.Is_Empty then
         for index in Boolean_2D.First_Index .. Boolean_2D.Last_Index loop
            Weights.Append (1.0);
         end loop;
      end if;

      Classifier_Types.Check_Length ("Classification_Metrics.Average",
                                     Boolean_2D, Weights);
      for index in Boolean_2D.First_Index .. Boolean_2D.Last_Index loop
         Values := Boolean_2D.Element (index);
         for index_2 in Values.First_Index .. Values.Last_Index loop
            Value := Values (index_2);
            Sum_Weights := Sum_Weights + Weights.Element (index_2);
            if Value.Value_Kind = Boolean_Type then
               if Value.Boolean_Value then
                  Result := Result + Weights.Element (index_2);
               end if;
            end if;
         end loop;
      end loop;

      return Result / Sum_Weights;

   end Average;

   --  ------------------------------------------------------------------------

   function Sum (Sample_Score : IL_Types.Value_Data_Lists_2D) return float is
      use IL_Types;
      Values : Value_Data_List;
      Value  : Value_Record;
      Result : Float := 0.0;
   begin
      for index in Sample_Score.First_Index .. Sample_Score.Last_Index loop
         Values := Sample_Score.Element (index);
         for index_2 in Values.First_Index .. Values.Last_Index loop
            Value := Values (index_2);
            case Value.Value_Kind is
               when Boolean_Type =>
                  if Value.Boolean_Value then
                     Result := Result + 1.0;
                  end if;
               when Float_Type =>
                  Result := Result + Value.Float_Value;
               when Integer_Type =>
                  Result := Result + Float (Value.Integer_Value);
               when UB_String_Type => null;
            end case;
         end loop;
      end loop;

      return Result;

   end Sum;

   --  ------------------------------------------------------------------------

   function Weighted_Sum
     (Sample_Score   : IL_Types.Value_Data_Lists_2D;
      Sample_Weights : Weights.Weight_List :=
        Classifier_Types.Float_Package.Empty_Vector;
      Normalize      : Boolean := False) return float is
      Routine_Name : constant String := "Classification_Metrics.Weighted_Sum ";
      Result       : Float := 0.0;
   begin
      Assert (not Sample_Score.Is_Empty, Routine_Name &
                "Sample_Score is empty");
      if Normalize then
         Result := Average (Sample_Weights, Sample_Score);
      elsif not Sample_Weights.Is_Empty then
         Classifier_Types.Check_Length
           (Routine_Name, Sample_Weights, Sample_Score);
         Result := Classifier_Types.Dot (Sample_Weights, Sample_Score);
      else
         Result := Sum (Sample_Score);
      end if;

      return Result;

   end Weighted_Sum;

   --  ------------------------------------------------------------------------

end Classification_Metrics;
