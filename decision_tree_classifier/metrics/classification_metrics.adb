
with Ada.Assertions; use Ada.Assertions;
with Ada.Containers;

package body Classification_Metrics is

   function Weighted_Sum
     (Sample_Score  : ML_Types.Value_Data_Lists_2D;
      Sample_Weight : Weights.Weight_List :=
        Classifier_Types.Float_Package.Empty_Vector;
      Normalize     : Boolean := False) return float;

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

   --     function Average (L, R : ML_Types.Value_Data_Lists_2D)
   --                       return ML_Types.Value_Record is
   --        use ML_Types;
   --        L_Data_List : Value_Data_List := L.First_Element;
   --        R_Data_List : Value_Data_List;
   --        Result      : Value_Record (L_Data_List.First_Element.Value_Kind);
   --     begin
   --        for index in L.First_Index .. L.Last_Index loop
   --           L_Data_List := L.Element (index);
   --           R_Data_List := R.Element (index);
   --           for index in L_Data_List.First_Index .. L_Data_List.Last_Index loop
   --              Result := Result +
   --                L_Data_List.Element (index) * R_Data_List.Element (index);
   --           end loop;
   --        end loop;
   --
   --        return Result;
   --
   --     end Average;

   --  ----------------------------------------------------------------------------

   function Accuracy_Score
     (Y_True, Y_Prediction : ML_Types.Value_Data_Lists_2D;
      Normalize            : Boolean := True;
      Sample_Weight        : Weights.Weight_List :=
        Classifier_Types.Float_Package.Empty_Vector)
      return float is
      use Ada.Containers;
      use ML_Types;
      Routine_Name : constant String := "Classification_Metrics.Accuracy_Score";
      Score        : Value_Data_Lists_2D;
   begin
      Assert (Y_True.Length = Y_Prediction.Length, Routine_Name &
                "");
      Assert (Y_True.Length = Sample_Weight.Length, Routine_Name &
                "");
      Score := Y_Prediction = Y_True;

      return Weighted_Sum (Score, Sample_Weight, Normalize);

   end Accuracy_Score;

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
      Result       : Float := 0.0;
   begin
      if Normalize then
         null;
         --           Result := Average (Sample_Score, Sample_Weight);
      elsif not Sample_Weight.Is_Empty then
         Result := Average (Sample_Weight, Sample_Score);
      else
         Result := Sum (Sample_Score);
      end if;

      return Result;

   end Weighted_Sum;

   --  ------------------------------------------------------------------------

end Classification_Metrics;
