
with Ada.Containers;

with Maths;

with ML_Types;

package body Criterion is

    --  ------------------------------------------------------------------------

    procedure Children_Impurity (Criteria                      : Criterion_Class;
                                 Impurity_Left, Impurity_Right : out Float) is
        use Maths.Float_Math_Functions;
        Class_List     : ML_Types.Value_Data_List;
        Left_List      : Classifier_Types.Weight_List;
        Right_List     : Classifier_Types.Weight_List;
        Count_K        : Float;
        Entropy_Left   : Float := 0.0;
        Entropy_Right  : Float := 0.0;
    begin
        for k in 1 .. Criteria.Num_Outputs loop
            Class_List := Criteria.Classes.Element (k);
            Left_List := Criteria.Sum_Left.Element (k);
            Right_List := Criteria.Sum_Right.Element (k);
            for c in Class_List.First_Index .. Class_List.Last_Index loop
                Count_K := Left_List.Element (c);
                if Count_K > 0.0 then
                    Count_K := Count_K / Criteria.Weighted_Left;
                    Entropy_Left := Entropy_Left - Count_K * Log (Count_K);
                end if;

                Count_K := Right_List.Element (c);
                if Count_K > 0.0 then
                    Count_K := Count_K / Criteria.Weighted_Right;
                    Entropy_Right := Entropy_Right - Count_K * Log (Count_K);
                end if;
            end loop;
        end loop;

        Impurity_Left := Entropy_Left / Float (Criteria.Num_Outputs);
        Impurity_Right := Entropy_Right / Float (Criteria.Num_Outputs);

    end Children_Impurity;

    --  ------------------------------------------------------------------------
    --  Node_Impurity evaluates the Gini criterion as the impurity of the
    --  current node
    function Gini_Node_Impurity (Criteria : in out Criterion_Class)
                                return Float is
        use Ada.Containers;
        Sum_Total : Classifier_Types.Weight_List;
        Count_K   : Float;
        Gini      : Float := 0.0;
        Sq_Count  : Float := 0.0;
    begin
        for index_k in Positive range 1 .. Criteria.Num_Outputs loop
            Sq_Count := 0.0;
            if Criteria.Classes.Length > Sum_Total.Length then
                Sum_Total.Set_Length (Criteria.Classes.Length);
            end if;

            for Class_Index in Criteria.Classes.First_Index ..
              Criteria.Classes.Last_Index loop
                Count_K := Float (Sum_Total.Element (Class_Index));
                Sq_Count := Sq_Count + Count_K ** 2;
            end loop;

            Gini := Gini +
              1.0 - Sq_Count / Float (Criteria.Num_Weighted_Node_Samples ** 2);
        end loop;

        return Gini / Float (Criteria.Num_Outputs);

    end Gini_Node_Impurity;

    --  ------------------------------------------------------------------------

    function Impurity_Improvement
      (Criteria                                       : Criterion_Class;
       Impurity_Parent, Impurity_Left, Impurity_Right : Float) return float is
    begin

        return (Criteria.Weighted_Node_Samples / Criteria.Weighted_Samples) *
          (Impurity_Parent -
             (Criteria.Weighted_Right / Criteria.Weighted_Node_Samples *
                  Impurity_Right) -
             (Criteria.Weighted_Left / Criteria.Weighted_Node_Samples *
                  Impurity_Left));

    end Impurity_Improvement;

    --  ------------------------------------------------------------------------
    --  L742
    procedure Init (Criteria         : in out Criterion_Class;
                    Y                : ML_Types.List_Of_Value_Data_Lists;
                    Samples          : ML_Types.List_Of_Value_Data_Lists;
                    --  Sample_Weight contains the weight of each sample
                    Sample_Weight    : Classifier_Types.Weight_List;
                    Weighted_Samples : Float;
                    Start, Stop      : Natural) is
        Y_I         : ML_Types.Value_Data_List;
        Sum_Total   : Classifier_Types.Weight_List;
        Weight      : Float := 1.0;
        Y_Ik        : Float;
        W_Ik        : Float;
    begin
        Criteria.Y := Y;
        Criteria.Sample_Weight := Sample_Weight;
        Criteria.Samples := Samples;
        Criteria.Start := Start;
        Criteria.Stop := Stop;
        Criteria.Weighted_Samples := Weighted_Samples;
        Criteria.Num_Weighted_Node_Samples := 0;

        Criteria.Sq_Sum_Total := 0.0;
        Criteria.Sum_Total.Clear;

        for k in 1 .. Criteria.Num_Outputs loop
            Sum_Total.Append (0.0);
            for index in 1 .. Criteria.Classes.Length loop
                Sum_Total.Append (0.0);
            end loop;
            Criteria.Sum_Total.Append (Sum_Total);
        end loop;

        for p in Start .. Stop loop
            Y_I := Samples.Element (p);

            --  Weight is originally set to be 1.0, meaning that if no
            --  sample weights are given, the default weight of each sample is 1.0
            if not Sample_Weight.Is_Empty then
                Weight := Sample_Weight.Element (p);
            end if;

            for k in 1 .. Criteria.Num_Outputs loop
                Y_Ik := Float (Y_I.Element (k).Integer_Value);
                W_Ik := Y_Ik * Weight;
                Sum_Total.Replace_Element
                  (k, Sum_Total.Element (k) + W_Ik);
                Criteria.Sq_Sum_Total := Criteria.Sq_Sum_Total + Y_Ik * W_Ik;
            end loop;

            Criteria.Weighted_Node_Samples :=
              Criteria.Weighted_Node_Samples + Weight;
        end loop;

        Reset (Criteria);

    end Init;

    --  ------------------------------------------------------------------------
    --  Node_Impurity evaluate the cross-entropy criterion as impurity of the
    --  current node. i.e. the impurity of samples[start:end].
    --  The smaller the impurity the better.
    function Node_Impurity (Self : Criterion_Class) return Float is
        use Maths.Float_Math_Functions;
        Class_List     : ML_Types.Value_Data_List;
        Sum_Total_List : Classifier_Types.Weight_List;
        Count_K        : Float := 0.0;
        Entropy        : Float := 0.0;
    begin
        for index in Self.Classes.First_Index .. Self.Classes.Last_Index loop
            Class_List := Self.Classes.Element (index);
            Sum_Total_List := Self.Sum_Total.Element (index);
            for c in Class_List.First_Index .. Class_List.Last_Index loop
                Count_K := Sum_Total_List.Element (c);
                if Count_K > 0.0 then
                    Count_K := Count_K / Self.Weighted_Node_Samples;
                    Entropy := Entropy - Count_K * Log (Count_K);
                end if;
            end loop;
        end loop;
        return Entropy / Float (Self.Num_Outputs);
    end Node_Impurity;

    --  -------------------------------------------------------------------------

    procedure Node_Value (Self : Criterion_Class;
                          Value : out Classifier_Types.Float_List) is
        Sum_Total_List : Classifier_Types.Weight_List;
    begin
        Value.Clear;
        for index in Self.Sum_Total.First_Index .. Self.Sum_Total.Last_Index loop
            Sum_Total_List := Self.Sum_Total.Element (index);
            for s in Sum_Total_List.First_Index .. Sum_Total_List.Last_Index loop
                Value.Append (Sum_Total_List.Element (s) /
                                Self.Weighted_Node_Samples);
            end loop;
        end loop;

    end Node_Value;

    --  -------------------------------------------------------------------------

    function Proxy_Impurity_Improvement (Criteria : Criterion_Class)
                                        return Float is
        Impurity_Left  : Float;
        Impurity_Right : Float;
    begin
        Children_Impurity (Criteria, Impurity_Left, Impurity_Right);
        return -Criteria.Weighted_Right * Impurity_Right -
          Criteria.Weighted_Left * Impurity_Left;

    end Proxy_Impurity_Improvement;

    --  ------------------------------------------------------------------------

    procedure Reset (Criteria : in out Criterion_Class) is
        Left_List  : Classifier_Types.Weight_List;
        Right_List : Classifier_Types.Weight_List;
    begin
        Criteria.Sum_Left.Clear;
        Criteria.Sum_Right.Clear;
        for k in 1 .. Criteria.Num_Outputs loop
            Left_List := Criteria.Sum_Left.Element (k);
            Right_List := Criteria.Sum_Right.Element (k);
            for s in Left_List.First_Index .. Left_List.Last_Index loop
                Left_List.Append (0.0);
                Right_List.Append (Criteria.Sum_Total.Element (k));
            end loop;
            Criteria.Sum_Left.Append (Left_List);
            Criteria.Sum_Right.Append (Left_List);
        end loop;

        Criteria.Weighted_Left := 0.0;
        Criteria.Weighted_Right := Criteria.Weighted_Node_Samples;

    end Reset;

    --  ------------------------------------------------------------------------

    procedure Reverse_Reset (Criteria : in out Criterion_Class) is
        Left_List  : Classifier_Types.Weight_List;
        Right_List : Classifier_Types.Weight_List;
    begin
        Criteria.Sum_Left.Clear;
        Criteria.Sum_Right.Clear;
        for index in 1 .. Criteria.Num_Outputs loop
            Left_List := Criteria.Sum_Left.Element (index);
            Right_List := Criteria.Sum_Right.Element (index);
            for s in Left_List.First_Index .. Left_List.Last_Index loop
                Right_List.Append (0.0);
                Left_List.Append (Criteria.Sum_Total (index));
            end loop;
            Criteria.Sum_Left.Append (Left_List);
            Criteria.Sum_Right.Append (Left_List);
        end loop;
    end Reverse_Reset;

    --  ------------------------------------------------------------------------
    --  Update statistics by moving samples[pos:new_pos] to the left child.
    procedure Update (Criteria : in out Criterion_Class;
                      New_Pos  : Positive) is
        use ML_Types;
        use Classifier_Types;
        use Float_Package;
        use Weight_List_Package;
        Left_List   : Weight_List;
        Right_List  : Weight_List;
        Total_List  : Weight_List;
        Sum         : Float;
        i           : Positive;
        Values      : Value_Data_List;
        Weight      : Float := 1.0;
    begin
        --  L439
        if (New_Pos - Criteria.Position) <= (Criteria.Stop - New_Pos) then
            for p in Criteria.Position .. New_Pos loop
                i := Criteria.Sample_Indices.Element (p);
                if not Criteria.Sample_Weight.Is_Empty then
                    Weight := Criteria.Sample_Weight.Element (i);
                end if;

                Values := Criteria.Y.Element (i);
                for k in Criteria.Sum_Left.First_Index ..
                  Criteria.Sum_Left.Last_Index loop
                    Left_List := Criteria.Sum_Left.Element (k);
                    for s in Left_List.First_Index .. Left_List.Last_Index loop
                        Sum := Left_List.Element (s);
                        Sum := Sum + Values.Element (s).Float_Value * Weight;
                        Left_List.Replace_Element (s, Sum);
                    end loop;
                    Criteria.Sum_Left.Replace_Element (k, Left_List);
                end loop;

                Criteria.Weighted_Left := Criteria.Weighted_Left + Weight;
            end loop;

        else
            Reverse_Reset (Criteria);
            for p in reverse Criteria.Stop .. New_Pos loop
                i := Criteria.Sample_Indices.Element (p);
                if not Criteria.Sample_Weight.Is_Empty then
                    Weight := Criteria.Sample_Weight.Element (i);
                end if;

                Values := Criteria.Y.Element (i);
                for k in Criteria.Sum_Left.First_Index ..
                  Criteria.Sum_Left.Last_Index loop
                    Left_List := Criteria.Sum_Left.Element (k);
                    for s in Left_List.First_Index .. Left_List.Last_Index loop
                        Sum := Left_List.Element (s);
                        Sum := Sum - Values.Element (s).Float_Value * Weight;
                        Left_List.Replace_Element (s, Sum);
                    end loop;
                    Criteria.Sum_Left.Replace_Element (k, Left_List);
                end loop;

                Criteria.Weighted_Left := Criteria.Weighted_Left - Weight;
            end loop;
        end if;

        --  Update right part statistics
        Criteria.Weighted_Right := Criteria.Weighted_Node_Samples -
          Criteria.Weighted_Left;
        for k in 1 .. Criteria.Num_Outputs loop
            Right_List := Criteria.Sum_Right.Element (k);
            Total_List := Criteria.Sum_Total.Element (k);
            for s in Right_List.First_Index .. Right_List.Last_Index loop
                Sum := Right_List.Element (s);
                Sum := Sum - Values.Element (s).Float_Value * Weight;
                Right_List.Replace_Element (s, Total_List.Element (s) - Sum);
            end loop;
            Criteria.Sum_Right.Replace_Element (k, Right_List);
        end loop;

    end Update;

    --  ------------------------------------------------------------------------

end Criterion;
