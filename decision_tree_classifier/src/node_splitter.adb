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
            if not Sample_Weight.Is_Empty then
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

    procedure Init_Split (theSplit : in out Split_Record; Start : Natural) is
    begin
        theSplit.Impurity_Left := Float'Last;
        theSplit.Impurity_Right := Float'Last;
        theSplit.Improvement := -Float'Last;
        theSplit.Pos := Start;
        theSplit.Feature_Index := 1;
        theSplit.Threshold := 0;

    end Init_Split;

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
      (Split  : in out Splitter_Class; Start, Stop : Natural;
       Weighted_Node_Samples : in out Float) is
    begin
        Split.Start := Start;
        Split.Stop := Stop;

      Criterion.Init (Split.Criteria, Split.Y, Split.Sample_Weight,
                      Split.Weighted_Samples, Start, Stop,
                      Split.Sample_Indices);

            Weighted_Node_Samples := Split.Criteria.Weighted_Node_Samples;

    end Reset_Node;

    --  -------------------------------------------------------------------------
    --  BestSplitter.Split_Node samples up to max_features without replacement
    --  using a Fisher-Yates-based algorithm (using the local variables `f_i`
    --  and `f_j` to compute a permutation of the `features` array).
    procedure Split_Node (Self                  : in out Splitter_Class;
                          Impurity : Float;
                          theSplit              : in out Split_Record;
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
        Start_Positive       : Natural;
        Stop_Negative        : Natural;
        F_I                  : Natural := Num_Features;
        F_J                  : Natural;
        P_Index              : Positive;
        Swap                 : Natural;
        Threshold            : Value_Record;
    begin
        --  Skip CPU intensive evaluation of the impurity criterion for
        --  features that have already been detected as constant
        --  (hence not suitable for good splitting) by ancestor nodes and save
        --  the information on newly discovered constant features to avoid
        --  computation on descendant nodes.
        Init_Split (theSplit, Start_Positive);

        while F_I > Num_Total_Constants and
          (Num_Visited_Features < Max_Features or
             --   At least one drawn features must be non constant.
             Num_Visited_Features <= Num_Found_Constants + Num_Drawn_Constants)
        loop
            Num_Visited_Features := Num_Visited_Features + 1;
            --   Draw a feature at random;
            F_J := Num_Drawn_Constants +
              Maths.Random_Integer mod (F_I - Num_Found_Constants);

            if F_J < Num_Known_Constants then
                Swap := Num_Drawn_Constants;
                Features.Replace_Element
                  (Num_Drawn_Constants, Features.Element (F_J));
                Features.Replace_Element (F_J, Features.Element (Swap));
                Num_Drawn_Constants := Num_Drawn_Constants + 1;

            else
                F_J := F_J + Num_Found_Constants;
                Current.Feature_Index := Features.Element (F_J);
                Features_X.Clear;
                for index in Self.Start .. Self.Stop loop
                    Features_X.Append
                      (self.Feature_Values.Element (Current.Feature_Index));
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
                    Swap := Features.Element (F_J);
                    Features.Replace_Element
                      (F_J, Features.Element (Num_Total_Constants));
                    Features.Replace_Element (Num_Total_Constants, Swap);
                    Num_Found_Constants := Num_Found_Constants + 1;
                    Num_Total_Constants := Num_Total_Constants + 1;
                else
                    F_I := F_I - 1;
                    Swap := Features.Element (F_I);
                    Features.Replace_Element (F_I,
                                              Features.Element (F_J));
                    Features.Replace_Element (F_J, Swap);
                    --  Evaluate all splits
                    Criterion.Reset (Self.Criteria);
                    P_Index := Self.Start;
                    while P_Index <= Self.Stop loop
                        while P_Index + 1 <= Self.Stop and
                          Features.Element (P_Index + 1) <=
                          Features.Element (P_Index) loop
                            P_Index := P_Index + 1;
                        end loop;

                        P_Index := P_Index + 1;
                        if P_Index < Self.Stop then
                            Current.Pos := P_Index;
                            --  Reject if Min_Leaf_Samples is not guaranteed
                            if (Current.Pos - Self.Start) >=
                              Self.Min_Leaf_Samples and
                              (Self.Stop - Current.Pos) >=
                              Self.Min_Leaf_Samples then
                                Null;
                                --  self.criterion.update(current.pos)
                            end if;
                        end if;
                    end loop;
                end if;
            end if;

        end loop;

    end Split_Node;

end Node_Splitter;
