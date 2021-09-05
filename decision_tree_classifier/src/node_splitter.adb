--  Based on scikit-learn/sklearn/tree _splitter.pyx class BestSplitter

with Maths;

package body Node_Splitter is

   procedure Init (Self          : in out Split_Class;
                   X, Y          : ML_Types.List_Of_Value_Data_Lists;
                   Sample_Weight : Classifier_Types.Weight_List) is
      Num_Samples      : constant Positive :=Positive (X.Element (1).Length);
      Samples          : Classifier_Types.Natural_List;
      Weighted_Samples : Float := 0.0;
      J                : Natural := 0;
   begin
      for index in 1 .. Num_Samples loop
         if Sample_Weight.Is_Empty then
            if Sample_Weight.Element (index)  > 0.0 then
               Samples.Append (index);
               J := J + 1;
            end if;
            Weighted_Samples :=
              Weighted_Samples + Sample_Weight.Element (index);
         else
            Weighted_Samples := Weighted_Samples + 1.0;
         end if;
      end loop;

      Self.Num_Samples := J;
      Self.Weighted_Samples := Weighted_Samples;

   end Init;

   --  -------------------------------------------------------------------------

   procedure Reset_Node (Self                  : in out Split_Class; Start, Stop : Natural;
                         Weighted_Node_Samples : in out Classifier_Types.Weight_List) is
   begin
      Self.Start := Start;
      Self.Stop := Stop;

      Self.Criteria.Y := Self.Y;
      Self.Criteria.Sample_Weight := Self.Sample_Weight;
      Self.Criteria.Weighted_Node_Samples := Self.Weighted_Samples;
      Self.Criteria.Start := Self.Start;
      Self.Criteria.Stop := Self.Stop;
      Self.Criteria.Sample_Indices := Self.Sample_Indices;

      if Weighted_Node_Samples.Is_Empty then
         Weighted_Node_Samples.Append (Self.Criteria.Weighted_Node_Samples);
      else
         Weighted_Node_Samples.Replace_Element
           (1, Self.Criteria.Weighted_Node_Samples);
      end if;

   end Reset_Node;

   --  -------------------------------------------------------------------------

   procedure Split_Node (Self                  : Split_Class; Impurity : Float;
                         theSplit              : Split_Record;
                         Num_Constant_Features : ML_Types.Value_Data_List) is
      use Maths.Float_Math_Functions;
      use ML_Types;
      use ML_Types.Value_Data_Package;
      use ML_Types.Value_Data_Sorting;
      Features             : Classifier_Types.Natural_List :=
                               Self.Feature_Indices;
      Features_X           : Value_Data_List := Self.Feature_Values;
      Node_Features_X      : Value_Data_List;
      Current              : Split_Record;
      Num_Features         : Natural := Natural (Features.Length);
      Max_Features         : Natural := Self.Max_Features;
      Num_Known_Constants  : Natural :=
                               Num_Constant_Features.Element (1).Integer_Value;
      Num_Total_Constants  : Natural := Num_Known_Constants;
      Num_Visited_Features : Natural := 0;
      Num_Found_Constants  : Natural := 0;
      Num_Drawn_Constants  : Natural := 0;
      F_Index              : Natural := Num_Features;
      J_Index              : Natural;
   begin
      --  Sample up to Max_Features without replacement using a
      --  Fisher-Yates-based algorithm.
      --  Skip CPU intensive evaluation of the impurity criterion for
      --  features that have already been detected as constant
      --  (hence not suitable for good splitting) by ancestor nodes and save
      --  the information on newly discovered constant features to spare
      --  computation on descendant nodes.

      while F_Index > Num_Total_Constants and
        (Num_Visited_Features < Max_Features or
           Num_Visited_Features <= Num_Found_Constants + Num_Drawn_Constants)
      loop
         Num_Visited_Features := Num_Visited_Features + 1;
         J_Index := Num_Drawn_Constants +
           Maths.Random_Integer mod (F_Index - Num_Found_Constants);
         if J_Index < Num_Known_Constants then
            Features.Replace_Element
              (Num_Drawn_Constants, Features.Element (J_Index));
            Features.Replace_Element
              (J_Index, Features.Element (Num_Drawn_Constants));
            Num_Drawn_Constants := Num_Drawn_Constants + 1;
         else
            J_Index := J_Index + Num_Found_Constants;
            Current.Feature_Index := Features.Element (F_Index);
            Features_X.Clear;
            for index in Self.Start .. Self.Stop loop
               Features_X.Replace_Element
                 (Features_X.To_Cursor (Self.Sample_Indices.Element (index)),
                  self.Feature_Values.Element (Current.Feature_Index));
            end loop;

            Node_Features_X.Clear;
            for index in Self.Start .. Self.Stop loop
               Node_Features_X.Append (Features_X.Element (index));
            end loop;

            Sort (Node_Features_X);
            for index in Self.Start .. Self.Stop loop
               Features_X.Replace_Element
                 (Features_X.To_Cursor (Self.Sample_Indices.Element (index)),
                  Node_Features_X.Element (Current.Feature_Index));
            end loop;
         end if;

      end loop;

   end Split_Node;

end Node_Splitter;
