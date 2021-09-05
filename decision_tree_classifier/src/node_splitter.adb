--  Based on scikit-learn/sklearn/tree _splitter.pyx class BestSplitter

with Ada.Containers;

with Maths;

package body Node_Splitter is

    Feature_Threshold : constant Float := 10.0 ** (-7);

    procedure Replacement_Sort (Self : Splitter_Class;
                                Data : in out ML_Types.Value_Data_List;
                                Current : Split_Record;
                                Start, Stop : Positive);

    procedure Replacement_Sort (Self : Splitter_Class;
                                Data : in out Classifier_Types.Natural_List;
                                Current : Split_Record;
                                Start, Stop : Positive);

    --  -------------------------------------------------------------------------

    procedure Init (Self          : in out Splitter_Class;
                    X, Y          : ML_Types.List_Of_Value_Data_Lists;
                    Sample_Weight : Classifier_Types.Weight_List) is
        use Ada.Containers;
        Num_Samples      : constant Positive := Positive (X.Element (1).Length);
        Num_Features     : constant Positive := Positive (X.Length);
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

        Self.Feature_Indices.Clear;
        for index in 1 .. Num_Features loop
            Self.Feature_Indices.Append (index);
        end loop;
        Self.Feature_Values.Clear;
        Self.Feature_Values.Set_Length (Count_Type (Num_Samples));
        Self.Constant_Features.Clear;
        Self.Constant_Features.Set_Length (Count_Type (Num_Features));

        Self.Y := Y;
        Self.Sample_Weight := Sample_Weight;

    end Init;

    --  -------------------------------------------------------------------------

    procedure Replacement_Sort (Self : Splitter_Class;
                                Data : in out ML_Types.Value_Data_List;
                                Current : Split_Record;
                                Start, Stop : Positive) is
        use ML_Types;
        use Value_Data_Package;
        use Value_Data_Sorting;
        Temp : Value_Data_List;
    begin
        for index in Self.Start .. Self.Stop loop
            Temp.Append (Data.Element (index));
        end loop;

        Sort (Temp);
        for index in Start .. Stop loop
            Data.Replace_Element
              (Data.To_Cursor (Self.Sample_Indices.Element (index)),
               Temp.Element (Current.Feature_Index));
        end loop;

    end Replacement_Sort;

    --  -------------------------------------------------------------------------

    procedure Replacement_Sort (Self : Splitter_Class;
                                Data : in out Classifier_Types.Natural_List;
                                Current : Split_Record;
                                Start, Stop : Positive) is
        use Classifier_Types;
        use Natural_Package;
        use Natural_Sorting;
        Temp : Natural_List;
    begin
        for index in Self.Start .. Self.Stop loop
            Temp.Append (Data.Element (index));
        end loop;

        Sort (Temp);
        for index in Start .. Stop loop
            Data.Replace_Element
              (Data.To_Cursor (Self.Sample_Indices.Element (index)),
               Temp.Element (Current.Feature_Index));
        end loop;

    end Replacement_Sort;

    --  -------------------------------------------------------------------------

    procedure Reset_Node
      (Self   : in out Splitter_Class; Start, Stop : Natural;
       Weighted_Node_Samples : in out Classifier_Types.Weight_List) is
        use Classifier_Types.Float_Package;
    begin
        Self.Start := Start;
        Self.Stop := Stop;

        Criterion.Reset (Self.Criteria);

        if Weighted_Node_Samples.Is_Empty then
            Weighted_Node_Samples.Append (Self.Criteria.Weighted_Node_Samples);
        else
            Weighted_Node_Samples.Replace_Element
              (1, Self.Criteria.Weighted_Node_Samples);
        end if;

    end Reset_Node;

    --  -------------------------------------------------------------------------

    procedure Split_Node (Self                  : in out Splitter_Class;
                          Impurity : Float;
                          theSplit              : Split_Record;
                          Num_Constant_Features : ML_Types.Value_Data_List) is
        use ML_Types;
        use ML_Types.Value_Data_Package;
        Num_Features         : constant Natural :=
                                 Natural (Self.Feature_Indices.Length);
        Samples              : Classifier_Types.Natural_List :=
                                 Self.Sample_Indices;
        Features             : Classifier_Types.Natural_List :=
                                 Self.Feature_Indices;
        Features_X           : Value_Data_List := Self.Feature_Values;
        Current              : Split_Record;
        Max_Features         : Natural := Self.Max_Features;
        Num_Known_Constants  : Natural
          :=  Num_Constant_Features.Element (1).Integer_Value;
        Num_Total_Constants  : Natural := Num_Known_Constants;
        Num_Visited_Features : Natural := 0;
        Num_Found_Constants  : Natural := 0;
        Num_Drawn_Constants  : Natural := 0;
        F_Index              : Natural := Num_Features;
        J_Index              : Natural;
        Swap                 : Natural;
        Threshold            : Value_Record;
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

                Replacement_Sort (Self, Features_X, Current, Self.Start,
                                  Self.Stop);
                Replacement_Sort (Self, Samples, Current, Self.Start,
                                  Self.Stop);
                if Features_X.First_Element.Value_Kind = Float_Type then
                    Threshold.Float_Value :=
                      Features_X.First_Element.Float_Value +
                        Feature_Threshold;
                else
                    Threshold := Features_X.First_Element;
                end if;

                if Features_X.Last_Element <= Threshold then
                    Swap := Features.Element (J_Index);
                    Features.Replace_Element
                      (J_Index, Features.Element (Num_Total_Constants));
                    Features.Replace_Element (Num_Total_Constants, Swap);
                    Num_Found_Constants := Num_Found_Constants + 1;
                    Num_Total_Constants := Num_Total_Constants + 1;
                else
                    F_Index := F_Index - 1;
                    Swap := Features.Element (F_Index);
                    Features.Replace_Element (F_Index,
                                              Features.Element (J_Index));
                    Features.Replace_Element (J_Index, Swap);
                    --  Evaluate all splits
                    Criterion.Reset (Self.Criteria);
                end if;
            end if;

        end loop;

    end Split_Node;

end Node_Splitter;
