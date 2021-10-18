--  Based on scikit-learn/sklearn/tree _splitter.pyx class BestSplitter

with Ada.Assertions;  use Ada.Assertions;
with Ada.Containers;
with Ada.Text_IO; use Ada.Text_IO;

with Maths;

with Classifier_Types;
with Classifier_Utilities;

package body Node_Splitter is

    Feature_Threshold : constant Float := 10.0 ** (-7);

    --  -------------------------------------------------------------------------

    procedure C_Init (Self          : in out Splitter_Class;
                      Criteria      : Criterion.Criterion_Class;
                      Max_Features  : Positive := 1;
                      Min_Leaf_Samples : Positive := 1;
                      Min_Leaf_Weight : Float := 0.0) is
    begin
        Self.Criteria := Criteria;
        Self.Max_Features := Max_Features;
        Self.Min_Leaf_Samples := Min_Leaf_Samples;
        Self.Min_Leaf_Weight := Min_Leaf_Weight;
    end C_Init;

    --  -------------------------------------------------------------------------

    procedure Init (Self             : in out Splitter_Class;
                    Input_X          : ML_Types.List_Of_Value_Data_Lists;
                    Y_Encoded        : Classifier_Types.List_Of_Natural_Lists;
                    Sample_Weight    : Weights.Weight_List;
                    Min_Leaf_Samples : Positive := 1) is
        use Ada.Containers;
        Num_Samples      : constant Positive := Positive (Input_X.Length);
        Num_Features     : constant Positive :=
                             Positive (Input_X.Element (1).Length);
        Weighted_Samples : Float := 0.0;
    begin
        Self.Min_Leaf_Samples := Min_Leaf_Samples;
        --  Min_Leaf_Samples is the minimum number of samples that each leaf can
        --  have, where splits which would result in having less samples in a
        --  leaf are not considered.
        Self.Sample_Indices.Clear;
        --  For each sample (row)
        for index in Input_X.First_Index .. Input_X.Last_Index loop
            --  Only work with positively weighted samples.
            if Sample_Weight.Is_Empty or else
              Sample_Weight.Element (index) > 0.0 then
                Self.Sample_Indices.Append (index);
            end if;

            if Sample_Weight.Is_Empty then
                Weighted_Samples := Weighted_Samples + 1.0;
            else
                Weighted_Samples :=
                  Weighted_Samples + Sample_Weight.Element (index);
            end if;
        end loop;

        --  Number of samples is the number of positively weighted samples.
        Self.Num_Samples := Natural (Self.Sample_Indices.Length);
        Self.Weighted_Samples := Weighted_Samples;

        Self.Feature_Indices.Clear;
        for index in 1 .. Num_Features loop
            Self.Feature_Indices.Append (index);
        end loop;

        Self.Feature_Values.Clear;
        Self.Feature_Values.Set_Length (Count_Type (Num_Samples));
        Self.Constant_Features_I.Clear;
        Self.Constant_Features_I.Set_Length (Count_Type (Num_Features));

        Self.X := Input_X;
        Self.Y := Y_Encoded;
        Self.Sample_Weight := Sample_Weight;

    end Init;

    --  -------------------------------------------------------------------------

    procedure Init_Split (theSplit : in out Split_Record; Start : Positive) is
    begin
        theSplit.Split_Row := Start + 1;
        theSplit.Impurity_Left := Float'Last;
        theSplit.Impurity_Right := Float'Last;
        theSplit.Improvement := -Float'Last;
        theSplit.Feature := 1;
        theSplit.Threshold := 0.0;

    end Init_Split;

    --  -------------------------------------------------------------------------

    function Node_Impurity (Self : Splitter_Class) return Float is
    begin
        return Criterion.Entropy_Node_Impurity (Self.Criteria);
    end Node_Impurity;

    --  -------------------------------------------------------------------------

    procedure Node_Value
      (Self   : Splitter_Class;
       Values : out Classifier_Types.List_Of_Float_Lists) is
    begin
        Criterion.Node_Value (Self.Criteria, Values);
    end Node_Value;

    --  -------------------------------------------------------------------------

    procedure Find_Best_Split (Self       : in out Splitter_Class;
                               Features_X : ML_Types.Value_Data_List;
                               Current    : in out Split_Record;
                               Best       : in out Split_Record) is
        use ML_Types;
        P_Index                   : Natural := Self.Start_Row;
        Current_Proxy_Improvement : Float := -Float'Last;
        Best_Proxy_Improvement    : Float := -Float'Last;
        LE                        : Boolean;
    begin
        if Current.Split_Row <= Self.Start_Row then
            raise Node_Splitter_Error with
              "Node_Splitter.Find_Best_Split start, current position" &
              Integer'Image (Current.Split_Row) &
              " should be greater than Start_Row" &
              Integer'Image (Self.Start_Row);
        end if;

        --  L379 Evaluate all splits
        Best := Current;
        Criterion.Reset (Self.Criteria);

        --  Set of features to be split :
        --  P_Index: Self.Start_Index through Self.End_Index
        while P_Index <= Self.End_Row loop
            LE := True;
            while P_Index + 1 < Self.End_Row and LE loop
                case Features_X.Element (P_Index + 1).Value_Kind is
                when Boolean_Type =>
                    --  if current X value is false and next X value is true
                    --  increment X index
                    LE := not Features_X.Element (P_Index + 1).Boolean_Value and
                      Features_X.Element (P_Index + 1).Boolean_Value;
                    if LE then
                        P_Index := P_Index + 1;
                    end if;
                when Float_Type =>
                    --  if current X is less than or or equal to next X
                    --  increment X index
                    LE := Features_X.Element (P_Index + 1).Float_Value <=
                      Features_X.Element
                        (P_Index).Float_Value + Feature_Threshold;
                    if LE then
                        P_Index := P_Index + 1;
                    end if;
                when Integer_Type =>
                    --  if current X is less than or or equal to next X
                    --  increment X index
                    LE := Features_X.Element (P_Index + 1).Integer_Value <=
                      Features_X.Element (P_Index).Integer_Value;
                    if LE then
                        P_Index := P_Index + 1;
                    end if;
                when UB_String_Type =>
                    --  if current X is less than or or equal to next X
                    --  increment X index
                    LE := Features_X.Element (P_Index + 1).UB_String_Value <=
                      Features_X.Element (P_Index).UB_String_Value;
                    if LE then
                        P_Index := P_Index + 1;
                    end if;
                end case;
            end loop; --  P1_Index

            P_Index := P_Index + 1;
            --  L393
            if P_Index < Self.End_Row then
                Best.Split_Row := P_Index;
                --  Best.Pos_I is the start index of the right node's data
                --  L398 Accept if min_samples_leaf is guaranteed
                if Current.Split_Row - Self.Start_Row >= Self.Min_Leaf_Samples and
                  Self.End_Row - Current.Split_Row >= Self.Min_Leaf_Samples then
                    --  L401
                    Criterion.Update (Self.Criteria, Best.Split_Row);

                    --  L405 Accept if min_weight_leaf is satisfied
                    if Self.Criteria.Num_Weighted_Left >= Self.Min_Leaf_Weight and
                      Self.Criteria.Num_Weighted_Right >= Self.Min_Leaf_Weight then
                        Current_Proxy_Improvement :=
                          Criterion.Proxy_Impurity_Improvement (Self.Criteria);
                        --  L412  Note: Improvements are negative values.
                        if Current_Proxy_Improvement > Best_Proxy_Improvement then
                            Best_Proxy_Improvement := Current_Proxy_Improvement;
                            --  L415
                            case Features_X.Element (P_Index).Value_Kind is
                            when Float_Type =>
                                Current.Threshold := 0.5 *
                                  (Features_X.Element (P_Index - 1).Float_Value
                                   + Features_X.Element (P_Index).Float_Value);
                                if Current.Threshold =
                                  Features_X.Element (P_Index).Float_Value or
                                  Current.Threshold = Float'Last or
                                  Current.Threshold = (-Float'Last) then
                                    case Features_X.Element
                                      (P_Index - 1).Value_Kind is
                                    when Float_Type =>
                                        Current.Threshold := Features_X.Element
                                          (P_Index - 1).Float_Value;
                                    when Integer_Type =>
                                        Current.Threshold := Float (Features_X.Element
                                                                    (P_Index - 1).Integer_Value);
                                    when Boolean_Type =>
                                        if Features_X.Element
                                          (P_Index - 1).Boolean_Value then
                                            Current.Threshold := 1.0;
                                        else
                                            Current.Threshold := 0.0;
                                        end if;
                                    when UB_String_Type => null;
                                    end case;
                                end if;

                            when Integer_Type =>
                                Current.Threshold := 0.5 * Float
                                  (Features_X.Element (P_Index - 1).Integer_Value
                                   + Features_X.Element (P_Index).Integer_Value);
                                if Current.Threshold =
                                  Float (Features_X.Element (P_Index).
                                             Integer_Value) or
                                    Current.Threshold = Float'Last or
                                    Current.Threshold = (-Float'Last) then
                                    Current.Threshold :=
                                      Float (Features_X.Element (P_Index - 1).
                                                 Integer_Value);
                                end if;

                            when Boolean_Type | UB_String_Type => null;
                            end case;

                            --  L422
                            Best.Threshold := Current.Threshold;
                        end if;
                    end if;
                end if;
            end if;
        end loop;

        if Best.Split_Row <= Self.Start_Row then
            raise Node_Splitter_Error with
              "Node_Splitter.Find_Best_Split, final position" &
              Integer'Image (Best.Split_Row) &
              " should be greater than Start_Index" &
              Integer'Image (Self.Start_Row);
        end if;

    end Find_Best_Split;

    --  -------------------------------------------------------------------------

    procedure Reorder_Rows (Self        : in out Splitter_Class;
                            Best_Split  : in out Split_Record;
                            X_Samples   : in out
                              Classifier_Types.Natural_List;
                            Impurity    : Float) is
        use Classifier_Types.Natural_Package;
        Partition_End : Natural;
        P_Index       : Positive;
        Sample_P      : Natural;
        X_1           : ML_Types.Value_Data_List;
        X             : ML_Types.Value_Record;
        Swap          : Positive;
        Crit          : Criterion.Criterion_Class;
    begin
        --  L425 Reorganize into samples[start:best.pos] + samples[best.pos:end]
        if Best_Split.Split_Row < Self.End_Row then
            Partition_End := Self.End_Row;
            P_Index := Self.Start_Row;
            while P_Index < Partition_End loop
                Sample_P := Self.Sample_Indices.Element (P_Index);
                X_1 := Self.X.Element (Sample_P);
                X := X_1.Element (Best_Split.Feature);

                case X.Value_Kind is
                when ML_Types.Float_Type =>
                    if X.Float_Value <=
                      Best_Split.Threshold then
                        P_Index := P_Index + 1;
                    else
                        Partition_End := Partition_End - 1;
                        Swap := X_Samples.Element (P_Index);
                        X_Samples.Replace_Element
                          (P_Index, X_Samples.Element (Partition_End));
                        X_Samples.Replace_Element (Partition_End, Swap);
                    end if;

                when ML_Types.Integer_Type =>
                    if Float (X.Integer_Value) <= Best_Split.Threshold then
                        P_Index := P_Index + 1;
                    else
                        Partition_End := Partition_End - 1;
                        Swap := X_Samples.Element (P_Index);
                        X_Samples.Replace_Element
                          (P_Index, X_Samples.Element (Partition_End));
                        X_Samples.Replace_Element (Partition_End, Swap);
                    end if;

                when others =>
                    raise Node_Splitter_Error with
                      "Node_Splitter.Reorder_Rows X.Value_Kind is invalid";
                end case;
            end loop;

            --  L436
            Criterion.Reset (Self.Criteria);
            Crit := Self.Criteria;
            Crit.Split_Row := Best_Split.Split_Row;
            Criterion.Update (Self.Criteria, Crit.Split_Row);
            Criterion.Gini_Children_Impurity
              (Self.Criteria, Best_Split.Impurity_Left, Best_Split.Impurity_Right);
            Best_Split.Improvement := Criterion.Impurity_Improvement
              (Self.Criteria, Impurity, Best_Split.Impurity_Left,
               Best_Split.Impurity_Right);
        end if;

    end Reorder_Rows;

    --  -------------------------------------------------------------------------
    --  Reset_Node resets the splitter Split based on node Split.Samples[start:end].
    procedure Reset_Node
      (Splitter              : in out Splitter_Class;
       Start_Row, End_Row    : Positive;
       Weighted_Node_Samples : in out Float) is
    begin
        if End_Row < Start_Row then
            raise Node_Splitter_Error with
              "Node_Splitter.Reset_Node, stop index" & Integer'Image (End_Row) &
              " should not be less than start index" & Integer'Image (End_Row);
        end if;

        Splitter.Start_Row := Start_Row;
        Splitter.End_Row := End_Row;
        Criterion.Classification_Init
          (Splitter.Criteria, Splitter.Y, Splitter.Sample_Indices,
           Splitter.Sample_Weight, Splitter.Weighted_Samples, Start_Row, End_Row);

        Weighted_Node_Samples := Splitter.Criteria.Num_Weighted_Node_Samples;

    end Reset_Node;

    --  -------------------------------------------------------------------------
    --  BestSplitter.Split_Node samples up to max_features without replacement
    --  using a Fisher-Yates-based algorithm (using the local variables `f_i`
    --  and `f_j` to compute a permutation of the `features` array).
    function Split_Node (Self                  : in out Splitter_Class;
                         Impurity              : Float;
                         Num_Constant_Features : in out Natural)
                         return Split_Record is
        use ML_Types;
        use Value_Data_Package;
        use Value_Data_Sorting;
        Num_Features         : constant Natural :=
                                 Natural (Self.Feature_Indices.Length);
        Max_Features         : constant Natural := Self.Max_Features;
        Start_Row            : constant Positive := Self.Start_Row;
        End_Row              : constant Positive := Self.End_Row;
        Features             : Classifier_Types.Natural_List :=
                                 Self.Feature_Indices;
        X_Features           : Value_Data_List;
        X_F_Start            : Value_Record;
        X_F_End              : Value_Record;
        Current_Split        : Split_Record;
        X_Samples_Row        : Natural;
        X_Samples            : Value_Data_List;
        F_I                  : Natural := Num_Features;
        F_J                  : Natural;
        Swap                 : Natural;
        Num_Known_Constants  : constant Natural := Num_Constant_Features;
        Num_Total_Constants  : Natural := Num_Known_Constants;
        Num_Visited_Features : Natural := 0;
        Num_Found_Constants  : Natural := 0;
        Num_Drawn_Constants  : Natural := 0;
        LE                   : Boolean;
        Best_Split           : Split_Record;
    begin
        Assert (End_Row > Start_Row, "Node_Splitter.Split_Node End_Row " &
                  Integer'Image (End_Row) & " should be greater than Start_Row "
                & Integer'Image (Start_Row));

        if Integer (Self.Sample_Indices.Length) = 0 then
            raise Node_Splitter_Error with
              "Node_Splitter.Split_Node called with empty Sample_Indices";
        end if;

        Init_Split (Current_Split, Start_Row);
        --          Classifier_Utilities.Print_Split_Record
        --            ("Node_Splitter.Split_Node Current_Split", Current_Split);

        --  L323
        while F_I > Num_Total_Constants and
          (Num_Visited_Features < Max_Features or
           --   At least one drawn feature must be non constant.
             Num_Visited_Features <=
               Num_Found_Constants + Num_Drawn_Constants) loop
            --  L329
            Num_Visited_Features := Num_Visited_Features + 1;
            --  L342
            --  Draw a feature at random;
            F_J := Num_Drawn_Constants + 1 +
              Maths.Random_Integer mod (F_I - Num_Found_Constants);

            if F_J < Num_Known_Constants then
                Swap := Num_Drawn_Constants;
                Features.Replace_Element
                  (Num_Drawn_Constants, Features.Element (F_J));
                Features.Replace_Element (F_J, Features.Element (Swap));
                Num_Drawn_Constants := Num_Drawn_Constants + 1;
            else
                --  L356
                F_J := F_J + Num_Found_Constants;
                if F_J = 0 then
                    F_J := 1;
                end if;

                --  L358 Sort samples along Current.Feature index;
                Current_Split.Feature := Features.Element (F_J);
                --  L364
                for index in Start_Row .. End_Row loop
                    X_Samples_Row := Self.Sample_Indices.Element (index);
                    X_Samples := Self.X.Element (X_Samples_Row);
                    --  X_Features is a Value_Data_List
                    X_Features.Append (X_Samples (Current_Split.Feature));
                end loop;

                --  L367
                --              Classifier_Utilities.Print_Value_Data_List
                --                ("Node_Splitter.Split_Node L367 X_Features", X_Features);
                Sort (X_Features);
                --              Classifier_Utilities.Print_Value_Data_List
                --                ("Node_Splitter.Split_Node L367 sorted X_Features", X_Features);
                --              Put_Line ("Node_Splitter.Split_Node L367 start, end: " &
                --                          Integer'Image (Start_Row) & ", " &
                --                          Integer'Image (End_Row));
                --  L369  Feature_Values is a value_data_list
                X_F_Start := X_Features.Element (X_Features.First_Index);
                --              Put_Line ("Node_Splitter.Split_Node L369 set X_F_End");
                X_F_End := X_Features.Element (X_Features.Last_Index);

                --              Put_Line ("Node_Splitter.Split_Node L369 case");
                case X_F_Start.Value_Kind is
                when Float_Type =>
                    LE := X_F_End.Float_Value  <= X_F_Start.Float_Value + Feature_Threshold;

                when Integer_Type =>
                    LE := X_F_End.Integer_Value <= X_F_Start.Integer_Value;

                when others =>
                    raise Node_Splitter_Error with
                      "Node_Splitter.Reset_Node, invalid X_Features" &
                      Data_Type'Image (X_F_Start.Value_Kind);
                end case;
                Self.Feature_Values := X_Features;

                --  Still L369
                if LE then
                    Swap := Features.Element (F_J);
                    Features.Replace_Element
                      (F_J, Features.Element (Num_Total_Constants + 1));
                    Features.Replace_Element (Num_Total_Constants + 1, Swap);

                    Num_Found_Constants := Num_Found_Constants + 1;
                    Num_Total_Constants := Num_Total_Constants + 1;

                else  --  L374
                    F_I := F_I - 1;
                    Swap := Features.Element (F_I);
                    Features.Replace_Element (F_I, Features.Element (F_J));
                    Features.Replace_Element (F_J, Swap);

                    --  Evaluate all splits
                    Criterion.Reset (Self.Criteria);
                    --  L381
                    Find_Best_Split (Self, Self.Feature_Values, Current_Split,
                                     Best_Split);
                end if;
            end if;
        end loop;

        --          Classifier_Utilities.Print_Split_Record
        --            ("Node_Splitter.Split_Node L421, ", Current_Split);
        --          Classifier_Utilities.Print_Split_Record
        --            ("Node_Splitter.Split_Node L421, Best_Split", Best_Split);

        --  L421  Reorganize into samples
        --        (start .. best.pos) + samples (best.pos .. end)
        Reorder_Rows (Self, Best_Split, Self.Sample_Indices, Impurity);
        Assert (Best_Split.Split_Row >= Start_Row,
                "Node_Splitter.Split_Node, invalid position" &
                  Integer'Image (Best_Split.Split_Row) &
                  " should be greater than Start_Row" &
                  Integer'Image (Self.Start_Row));

        --  L448
        --  Respect invariant for constant features: the original order of
        --  element in features[:n_known_constants] must be preserved for
        --  sibling and child nodes.
        if Num_Known_Constants > 0 then
            for index in 1 .. Num_Known_Constants loop
                Features.Replace_Element
                  (index, Self.Constant_Features_I.Element (index));
            end loop;

            --   Copy newly found constant features
            for index in Num_Known_Constants + 1 .. Num_Found_Constants loop
                Self.Constant_Features_I.Replace_Element
                  (index, Features.Element (index));
            end loop;

            Put_Line ("Node_Splitter.Split_Node L367");
            --  L367
            --              Classifier_Utilities.Print_Value_Data_List
            --                ("Node_Splitter.Split_Node L367 X_Features", X_Features);
            Sort (X_Features);
            Classifier_Utilities.Print_Value_Data_List
              ("Node_Splitter.Split_Node L367 sorted X_Features", X_Features);
            Put_Line ("Node_Splitter.Split_Node L367 start, end: " &
                        Integer'Image (Start_Row) & ", " &
                        Integer'Image (End_Row));
            Put_Line ("Node_Splitter.Split_Node L369");
            --  L369  Feature_Values is a value_data_list
            X_F_Start := X_Features.Element (X_Features.First_Index);
            --              Put_Line ("Node_Splitter.Split_Node L369 set X_F_End");
            X_F_End := X_Features.Element (X_Features.Last_Index);

            Put_Line ("Node_Splitter.Split_Node L369 case");
            case X_F_Start.Value_Kind is
               when Float_Type =>
                  LE := X_F_End.Float_Value  <= X_F_Start.Float_Value + Feature_Threshold;

               when Integer_Type =>
                  LE := X_F_End.Integer_Value <= X_F_Start.Integer_Value;

               when others =>
                  raise Node_Splitter_Error with
                    "Node_Splitter.Reset_Node, invalid X_Features" &
                    Data_Type'Image (X_F_Start.Value_Kind);
            end case;
            Self.Feature_Values := X_Features;

            Put_Line ("Node_Splitter.Split_Node still L369");
            --  Still L369
            if LE then
               Swap := Features.Element (F_J);
               Features.Replace_Element
                 (F_J, Features.Element (Num_Total_Constants + 1));
               Features.Replace_Element (Num_Total_Constants + 1, Swap);

               Num_Found_Constants := Num_Found_Constants + 1;
               Num_Total_Constants := Num_Total_Constants + 1;

            else  --  L374
               F_I := F_I - 1;
               Swap := Features.Element (F_I);
               Features.Replace_Element (F_I, Features.Element (F_J));
               Features.Replace_Element (F_J, Swap);

               --  Evaluate all splits
               Criterion.Reset (Self.Criteria);
               Put_Line ("Node_Splitter.Split_Node L381");
               --  L381
               Find_Best_Split (Self, Self.Feature_Values, Current_Split,
                                Best_Split);
            end if;
         end if;
      end loop;

      Classifier_Utilities.Print_Split_Record
        ("Node_Splitter.Split_Node L421, ", Current_Split);
      Classifier_Utilities.Print_Split_Record
        ("Node_Splitter.Split_Node L421, Best_Split", Best_Split);

      --  L421  Reorganize into samples
      --        (start .. best.pos) + samples (best.pos .. end)
      Reorder_Rows (Self, Best_Split, Self.Sample_Indices, Impurity);
      Assert (Best_Split.Split_Row >= Start_Row,
              "Node_Splitter.Split_Node, invalid position" &
                Integer'Image (Best_Split.Split_Row) &
                " should be greater than Start_Row" &
                Integer'Image (Self.Start_Row));

      --  L448
      --  Respect invariant for constant features: the original order of
      --  element in features[:n_known_constants] must be preserved for
      --  sibling and child nodes.
      if Num_Known_Constants > 0 then
         for index in 1 .. Num_Known_Constants loop
            Features.Replace_Element
              (index, Self.Constant_Features_I.Element (index));
         end loop;

         --   Copy newly found constant features
         for index in Num_Known_Constants + 1 .. Num_Found_Constants loop
            Self.Constant_Features_I.Replace_Element
              (index, Features.Element (index));
         end loop;
      end if;

      Num_Constant_Features := Num_Total_Constants;
      return Best_Split;

   end Split_Node;

end Node_Splitter;
