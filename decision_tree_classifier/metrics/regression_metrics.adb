
--  with Ada.Assertions; use Ada.Assertions;
--  with Ada.Text_IO; use Ada.Text_IO;

with Maths;

package body Regression_Metrics is

   procedure Square (Values : in out ML_Types.Value_Data_List);

   --  ------------------------------------------------------------------------

   function Average (Weight               : Classifier_Types.Float_List;
                     Y_True, Y_Prediction : ML_Types.Value_Data_Lists_2D)
                     return Float is
      use ML_Types;
      Diff        : constant ML_Types.Value_Data_Lists_2D :=
                      Y_Prediction - Y_True;
      Weights     : Classifier_Types.Float_List := Weight;
      Values      : Value_Data_List;
      Value       : Value_Record;
      Sum_Weights : Float := 0.0;
      Result      : Float := 0.0;
   begin
      if Weights.Is_Empty then
         for index in Diff.First_Index .. Diff.Last_Index loop
            Weights.Append (1.0);
         end loop;
      end if;

      Classifier_Types.Check_Length ("Regression_Metrics.Average",
                                     Diff, Weights);
      for index in Diff.First_Index .. Diff.Last_Index loop
         Values := Diff.Element (index);
         Square (Values);
         for index_2 in Values.First_Index .. Values.Last_Index loop
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

   function Mean_Squared_Error
     (Y_True, Y_Prediction : ML_Types.Value_Data_Lists_2D;
      Sample_Weight        : Weights.Weight_List :=
        Classifier_Types.Float_Package.Empty_Vector;
      Multioutput          : Multioutput_Type := MO_Uniform_Average;
      Squared              : Boolean := True)
      return Classifier_Types.Float_List is
      use Maths.Float_Math_Functions;
      use ML_Types;
      Routine_Name  : constant String :=
                        "Regression_Metrics.Mean_Squared_Error, ";
      Output_Errors : Float;
      Loss          : Classifier_Types.Float_List;
   begin
      Check_Lengths (Routine_Name & " True, Prediction: ",
                     Y_True, Y_Prediction);
      if not Sample_Weight.Is_Empty then
         Classifier_Types.Check_Length
           (Routine_Name & " True, Sample_Weight: ", Sample_Weight, Y_True);
      end if;

      Output_Errors := Average (Sample_Weight,Y_True, Y_Prediction);
      if not Squared then
         Output_Errors := Sqrt (Output_Errors);
      end if;

      case Multioutput is
         when MO_Raw_Values => Loss.Append (Output_Errors);
         when MO_Uniform_Average => null;
      end case;

      return Loss;

   end Mean_Squared_Error;

   --  ------------------------------------------------------------------------

   procedure Square (Values : in out ML_Types.Value_Data_List) is
      use ML_Types;
      Value  : Value_Record;
   begin
      for index in Values.First_Index .. Values.Last_Index loop
         Value := Values (index);
         case Value.Value_Kind is
            when Boolean_Type => null;
            when Float_Type =>
               Value.Float_Value := Value.Float_Value ** 2;
            when Integer_Type =>
               Value.Integer_Value := Value.Integer_Value ** 2;
            when UB_String_Type => null;
         end case;
      end loop;

   end Square;

   --  ------------------------------------------------------------------------

end Regression_Metrics;
