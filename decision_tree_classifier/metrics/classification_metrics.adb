
with Ada.Containers;
with Ada.Text_IO; use Ada.Text_IO;

with Printing;

package body Classification_Metrics is

   function Weighted_Sum
     (Sample_Score  : ML_Types.Value_Data_Lists_2D;
      Sample_Weight : Weights.Weight_List :=
        Classifier_Types.Float_Package.Empty_Vector;
      Normalize     : Boolean := False) return float;

   --  ------------------------------------------------------------------------

   function Accuracy_Score
     (Y_True, Y_Prediction : ML_Types.Value_Data_Lists_2D;
      Normalize            : Boolean := True;
      Sample_Weight        : Weights.Weight_List :=
        Classifier_Types.Float_Package.Empty_Vector)
      return float is
      use Ada.Containers;
      use ML_Types;
      Routine_Name : constant String :=
                       "Classification_Metrics.Accuracy_Score, ";
      Score        : Value_Data_Lists_2D;
   begin
      Check_Length (Routine_Name & " True, Prediction: ", Y_True, Y_Prediction);
      if not Sample_Weight.Is_Empty then
         Classifier_Types.Check_Length (Routine_Name & " True, Sample_Weight: ",
                                        Sample_Weight, Y_True);
      end if;

      Put_Line (Routine_Name & "Prediction length " &
                  Count_Type'Image (Y_Prediction.Length));
      Score := Y_Prediction = Y_True;
      Printing.Print_Value_Data_Lists_2D (Routine_Name & "Score ", Score);

      return Weighted_Sum (Score, Sample_Weight, Normalize);

   end Accuracy_Score;

   --  ------------------------------------------------------------------------

   function Average (Weight     : Classifier_Types.Float_List;
                     Boolean_2D : ML_Types.Value_Data_Lists_2D)
                     return Float is
      use ML_Types;
      Values : Value_Data_List;
      Value  : Value_Record;
      Result : Float := 0.0;
   begin
      for index in Boolean_2D.First_Index .. Boolean_2D.Last_Index loop
         Values := Boolean_2D.Element (index);
         for index_2 in Values.First_Index .. Values.Last_Index loop
            Value := Values (index_2);
            if Value.Value_Kind = Boolean_Type then
               if Value.Boolean_Value then
                  Result := Result + Weight.Element (index_2);
               end if;
            end if;
         end loop;
      end loop;

      return Result;

   end Average;

   --  ------------------------------------------------------------------------

   function Sum (Sample_Score : ML_Types.Value_Data_Lists_2D) return float is
      use ML_Types;
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
     (Sample_Score  : ML_Types.Value_Data_Lists_2D;
      Sample_Weight : Weights.Weight_List :=
        Classifier_Types.Float_Package.Empty_Vector;
      Normalize     : Boolean := False) return float is
      --        Routine_Name : constant String := "Classification_Metrics.Weighted_Sum";
      Result    : Float := 0.0;
   begin
      if Normalize then
         Result := Average (Sample_Weight, Sample_Score);
      elsif not Sample_Weight.Is_Empty then
         Result := Classifier_Types.Dot (Sample_Weight, Sample_Score);
      else
         Result := Sum (Sample_Score);
      end if;

      return Result;

   end Weighted_Sum;

   --  ------------------------------------------------------------------------

end Classification_Metrics;
