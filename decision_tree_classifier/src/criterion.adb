
with Ada.Assertions; use Ada.Assertions;
with Ada.Text_IO; use Ada.Text_IO;

with Maths;

with Classifier_Utilities;
with ML_Types;

package body Criterion is

    --  -------------------------------------------------------------------------
    --  L214 __cinit__
    procedure C_Init (Criteria : in out Criterion_Class;
                      Num_Outputs : Positive;
                      Classes  : ML_Types.List_Of_Value_Data_Lists) is
    begin
        --  L252
        Criteria.Num_Outputs := Num_Outputs;
        Criteria.Classes := Classes;

    end C_Init;

    --  ------------------------------------------------------------------------
    --  L59, L214, 280
    procedure Classification_Init
      (Criteria           : in out Criterion_Class;
       Y                  : Classifier_Types.List_Of_Natural_Lists;
       Sample_Indices     : Classifier_Types.Natural_List;
       --  Sample_Weight contains the weight of each sample
       Sample_Weight      : Weights.Weight_List;
       Weighted_Samples   : Float;
       Start_Row, End_Row : Natural) is
        Num_Outputs     : constant Positive := Positive (Y.Element (1).Length);
        Sum_Total_K     : Classifier_Types.Float_List;
        Y_I_Index       : Positive;
        Y_I             : Classifier_Types.Natural_List;
        Y_Ik            : Natural;
        Weight          : Float := 1.0;
    begin
        Put_Line ("Criterion.Classification_Init");
        Criteria.Y := Y;
        Criteria.Sample_Weight := Sample_Weight;
        Criteria.Sample_Indices := Sample_Indices;
        Criteria.Start_Row := Start_Row;
        Criteria.End_Row := End_Row;
        Criteria.Num_Weighted_Samples := Weighted_Samples;
        Criteria.Num_Weighted_Node_Samples := 0.0;

        Criteria.Sq_Sum_Total := 0.0;
        Criteria.Sum_Total.Clear;

        Put_Line ("Criterion.Classification_Init L325");
        Assert (not Criteria.Classes.Is_Empty,
                "Criterion.Classification_Init Classes is empty");
        --  L325
        for row in 1 .. Num_Outputs loop
            Sum_Total_K.Clear;
            for c in Criteria.Classes.Element (row).First_Index ..
              Criteria.Classes.Element (row).Last_Index loop
                Sum_Total_K.Append (0.0);
            end loop;
            Criteria.Sum_Total.Append (Sum_Total_K);
        end loop;

        Put_Line ("Criterion.Classification_Init L329");
        --  L329
        for p in Start_Row .. End_Row loop
            Y_I_Index := Sample_Indices.Element (p);

            --  Weight is originally set to be 1.0, meaning that if no
            --  sample weights are given, the default weight of each sample is 1.0
            if not Sample_Weight.Is_Empty then
                Weight := Sample_Weight.Element (Y_I_Index);
            end if;

            --  L338 Count weighted class frequency for each target
            Y_I := Y.Element (Y_I_Index);
            for k in 1 .. Num_Outputs loop
                Sum_Total_K := Criteria.Sum_Total.Element (k);
                Y_Ik := Y_I.Element (k);  --  Y_Ik corresponds to c
                Sum_Total_K.Replace_Element
                  (Y_Ik, Sum_Total_K.Element (Y_Ik) + Weight);
                Criteria.Sum_Total.Replace_Element (k, Sum_Total_K);
            end loop;

            Criteria.Num_Weighted_Node_Samples :=
              Criteria.Num_Weighted_Node_Samples + Weight;
        end loop;

        --          Classifier_Utilities.Print_Weights_Lists
        --            ("Criterion.Classification_Init, Criteria.Sum_Total",
        --             Criteria.Sum_Total);

        Reset (Criteria);

    end Classification_Init;

    --  ------------------------------------------------------------------------
    --  L637
    procedure Gini_Children_Impurity (Criteria       : Criterion_Class;
                                      Impurity_Left,
                                      Impurity_Right : out Float) is
        use Maths.Float_Math_Functions;
        Num_Outputs    : constant Positive :=
                           Positive (Criteria.Y.Element (1).Length);
        Class_List     : ML_Types.Value_Data_List;
        Sum_Left_K     : Classifier_Types.Float_List;
        Sum_Right_K    : Classifier_Types.Float_List;
        Count_K        : Float;
        Sq_Count_Left  : Float;
        Sq_Count_Right : Float;
    begin
        --  L662
        for k in Criteria.Classes.First_Index ..
          Criteria.Classes.Last_Index loop
            Sq_Count_Left := 0.0;
            Sq_Count_Right := 0.0;
            Class_List := Criteria.Classes.Element (k);
            Sum_Left_K := Criteria.Sum_Left.Element (k);
            Sum_Right_K := Criteria.Sum_Right.Element (k);
            for c in Class_List.First_Index .. Class_List.Last_Index loop
                Count_K := Sum_Left_K.Element (c);
                if Count_K > 0.0 then
                    Count_K := Count_K / Criteria.Num_Weighted_Left;
                    Sq_Count_Left := Sq_Count_Left - Count_K * Log (Count_K);
                end if;

                Count_K := Sum_Right_K.Element (c);
                if Count_K > 0.0 then
                    Count_K := Count_K / Criteria.Num_Weighted_Right;
                    Sq_Count_Right := Sq_Count_Right - Count_K * Log (Count_K);
                end if;
            end loop;
        end loop;

        Impurity_Left := Sq_Count_Left / Float (Num_Outputs);
        Impurity_Right := Sq_Count_Right / Float (Num_Outputs);

    end Gini_Children_Impurity;

    --  ------------------------------------------------------------------------
    --  L 608 Gini_Node_Impurity evaluates the Gini criterion as the impurity
    --   of the current node
    function Gini_Node_Impurity (Criteria : in out Criterion_Class)
                                 return Float is
        Num_Outputs   : constant Positive := Positive (Criteria.Y.Length);
        Sum_Total_K   : Weights.Weight_List;
        Count_K       : Float;
        Gini          : Float := 0.0;
        Sq_Count      : Float := 0.0;
    begin
        --  620
        for index_k in Criteria.Y.First_Index .. Criteria.Y.Last_Index loop
            Sq_Count := 0.0;
            for Class_Index in Criteria.Classes.First_Index ..
              Criteria.Classes.Last_Index loop
                Count_K := Float (Sum_Total_K.Element (Class_Index));
            end loop;
            Sq_Count := Sq_Count + Count_K ** 2;

            Gini := Gini +
              1.0 - Sq_Count / Float (Criteria.Num_Weighted_Node_Samples ** 2);
        end loop;

        return Gini / Float (Num_Outputs);

    end Gini_Node_Impurity;

    --  ------------------------------------------------------------------------
    --  L524 Entropy_Node_Impurity evaluate the cross-entropy criterion as impurity of the
    --  current node. i.e. the impurity of samples[start:end].
    --  The smaller the impurity the better.
    function Entropy_Node_Impurity (Self : Criterion_Class) return Float is
        use Maths.Float_Math_Functions;
        Class_List  : ML_Types.Value_Data_List;
        Sum_Total_K : Classifier_Types.Float_List;
        Count_K     : Float := 0.0;
        Entropy     : Float := 0.0;
    begin
        if Self.Classes.Is_Empty then
            raise Criterion_Error with
              "Criterion.Entropy_Node_Impurity Criterion Classes is empty";
        end if;

        --  L535 Y structure samples (rows) x outputs (columns)
        for index in Self.Y.Element (1).First_Index .. Self.Y.Element (1).Last_Index loop
            Class_List := Self.Classes.Element (index);
            Sum_Total_K := Self.Sum_Total.Element (index);
            for c in Class_List.First_Index .. Class_List.Last_Index loop
                Count_K := Sum_Total_K.Element (c);
                if Count_K > 0.0 then
                    Count_K := Count_K / Self.Num_Weighted_Node_Samples;
                    Entropy := Entropy - Count_K * Log (Count_K);
                end if;
            end loop;
        end loop;

        return Entropy / Float (Self.Sum_Total.Length);

    end Entropy_Node_Impurity;

    --  ------------------------------------------------------------------------

    function Impurity_Improvement (Criteria       : Criterion_Class;
                                   Impurity_Parent, Impurity_Left,
                                   Impurity_Right : Float) return float is
    begin

        return (Criteria.Num_Weighted_Node_Samples / Criteria.Num_Weighted_Samples) *
          (Impurity_Parent -
             (Criteria.Num_Weighted_Right / Criteria.Num_Weighted_Node_Samples *
                  Impurity_Right) -
             (Criteria.Num_Weighted_Left / Criteria.Num_Weighted_Node_Samples *
                  Impurity_Left));

    end Impurity_Improvement;

    --  ------------------------------------------------------------------------

    procedure Node_Value (Self  : Criterion_Class;
                          Value : out Classifier_Types.List_Of_Float_Lists) is
        Sum_Total_K : Classifier_Types.Float_List;
        Total_K     : Classifier_Types.Float_List;
    begin
        if Self.Sum_Total.Is_Empty then
            raise Criterion_Error with "Criterion.Node_Value Sum_Total is empty";
        end if;

        Value.Clear;
        for index in Self.Sum_Total.First_Index .. Self.Sum_Total.Last_Index loop
            Sum_Total_K := Self.Sum_Total.Element (index);
            Total_K.Clear;
            for c in Self.Classes.First_Index .. Self.Classes.Last_Index loop
                Total_K.Append (Sum_Total_K.Element (c) /
                                  Self.Num_Weighted_Node_Samples);
            end loop;
            Value.Append (Total_K);
        end loop;
        Classifier_Utilities.Print_List_Of_Float_Lists
          ("Criterion.Node_Value, Value",  Value);

    end Node_Value;

    --  -------------------------------------------------------------------------

    function Proxy_Impurity_Improvement (Criteria : Criterion_Class)
                                         return Float is
        Impurity_Left  : Float;
        Impurity_Right : Float;
    begin
        Gini_Children_Impurity (Criteria, Impurity_Left, Impurity_Right);
        return -Criteria.Num_Weighted_Right * Impurity_Right -
          Criteria.Num_Weighted_Left * Impurity_Left;

    end Proxy_Impurity_Improvement;

    --  ------------------------------------------------------------------------
    --  L348
    procedure Reset (Criteria : in out Criterion_Class) is
        Num_Outputs : constant Positive :=
                        Positive (Criteria.Y.Element (1).Length);
        Sum_Left_K  : Classifier_Types.Float_List;
    begin
        Criteria.Split_Row := Criteria.Start_Row;
        Criteria.Num_Weighted_Left := 0.0;
        Criteria.Num_Weighted_Right := Criteria.Num_Weighted_Node_Samples;

        Criteria.Sum_Left.Clear;
        Criteria.Sum_Right.Clear;
        for k in 1 .. Num_Outputs loop
            Sum_Left_K.Clear;
            for c in Criteria.Classes.Element (k).First_Index ..
              Criteria.Classes.Element (k).Last_Index loop
                Sum_Left_K.Append (0.0);
            end loop;
            Criteria.Sum_Left.Append (Sum_Left_K);
        end loop;
        Criteria.Sum_Right := Criteria.Sum_Total;

    end Reset;

    --  ------------------------------------------------------------------------
    --  L378
    procedure Reverse_Reset (Criteria : in out Criterion_Class) is
        Num_Outputs : constant Positive :=
                        Positive (Criteria.Y.Element (1).Length);
        Sum_Right_K : Classifier_Types.Float_List;
    begin
        Criteria.Split_Row := Criteria.End_Row;
        Criteria.Num_Weighted_Left := Criteria.Num_Weighted_Node_Samples;
        Criteria.Num_Weighted_Right := 0.0;

        Criteria.Sum_Right.Clear;
        for index in 1 .. Num_Outputs loop
            Sum_Right_K.Clear;
            for c in Criteria.Classes.Element (index).First_Index ..
              Criteria.Classes.Element (index).Last_Index loop
                Sum_Right_K.Append (0.0);
            end loop;
            Criteria.Sum_Right.Append (Sum_Right_K);
        end loop;
        Criteria.Sum_Left := Criteria.Sum_Total;

    end Reverse_Reset;

    --  ------------------------------------------------------------------------
    --  Update statistics by moving samples[pos:new_pos] to the left child.
    procedure Update (Criteria : in out Criterion_Class;
                      New_Pos  : Positive) is
        Num_Outputs : constant Positive :=
                        Positive (Criteria.Y.Element (1).Length);
        i           : Positive;
        Y_I         : Classifier_Types.Natural_List;
        Y_Ik        : Natural;  --  Class index
        Sum_Left_K  : Classifier_Types.Float_List;
        Sum_Right_K : Classifier_Types.Float_List;
        Sum_K       : Classifier_Types.Float_List;
        Weight      : Float := 1.0;
    begin
        --  L439
        if (New_Pos - Criteria.Split_Row) <= (Criteria.End_Row - New_Pos) then
            for p in Criteria.Split_Row .. New_Pos loop
                i := Criteria.Sample_Indices.Element (p);
                if not Criteria.Sample_Weight.Is_Empty then
                    Weight := Criteria.Sample_Weight.Element (i);
                end if;

                Y_I := Criteria.Y.Element (i);
                for k in 1 .. Num_Outputs loop
                    Sum_Left_K := Criteria.Sum_Left.Element (k);
                    Y_Ik := Y_I.Element (k);  --  Class index
                    Sum_Left_K.Replace_Element
                      (Y_Ik, Sum_Left_K.Element (Y_Ik) + Weight);
                    Criteria.Sum_Left.Replace_Element (k, Sum_Left_K);
                end loop;
                Criteria.Num_Weighted_Left := Criteria.Num_Weighted_Left + Weight;
            end loop;

        else  --  L452
            Reverse_Reset (Criteria);
            for p in reverse Criteria.End_Row .. New_Pos loop
                i := Criteria.Sample_Indices.Element (p);
                if not Criteria.Sample_Weight.Is_Empty then
                    Weight := Criteria.Sample_Weight.Element (i);
                end if;

                Y_I := Criteria.Y.Element (i);
                for k in 1 .. Num_Outputs loop
                    Y_Ik := Y_I.Element (k);
                    Sum_Left_K := Criteria.Sum_Left.Element (k);
                    Sum_Left_K.Replace_Element
                      (Y_Ik, Sum_Left_K.Element (Y_Ik) - Weight);
                    Criteria.Sum_Left.Replace_Element (k, Sum_Left_K);
                end loop;

                Criteria.Num_Weighted_Left := Criteria.Num_Weighted_Left - Weight;
            end loop;
        end if;

        --  L467 Update right part statistics
        Criteria.Num_Weighted_Right := Criteria.Num_Weighted_Node_Samples -
          Criteria.Num_Weighted_Left;
        for k in 1 .. Num_Outputs loop
            Sum_Left_K := Criteria.Sum_Left.Element (k);
            Sum_Right_K := Criteria.Sum_Right.Element (k);
            Sum_K := Criteria.Sum_Total.Element (k);
            for class_index in Criteria.Classes.Element (k).First_Index ..
              Criteria.Classes.Element (k).Last_Index loop
                Sum_Right_K.Replace_Element (class_index, Sum_K.Element (class_index) -
                                               Sum_Left_K.Element (class_index));
            end loop;
            Criteria.Sum_Right.Replace_Element (k, Sum_Right_K);
        end loop;
        Criteria.Split_Row := New_Pos;

    end Update;

    --  ------------------------------------------------------------------------

end Criterion;
