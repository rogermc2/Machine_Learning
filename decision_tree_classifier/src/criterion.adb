
with ML_Types;

package body Criterion is

    --  ------------------------------------------------------------------------
    --  Node_Impurity evaluates the Gini criterion as the impurity of the
    --  current node
    function Gini_Node_Impurity (Criteria : in out Criterion_Class)
                                 return Float is
        Sum_Total : constant Classifier_Types.Weight_List :=
                      Criteria.Sum_Total;
        Count_K   : Float;
        Gini      : Float := 0.0;
        Sq_Count  : Float := 0.0;
    begin
        for index_k in 1 .. Criteria.Num_Outputs loop
            Sq_Count := 0.0;
            if Natural (Criteria.Classes.Length) > Criteria.Stride then
                Criteria.Stride  := Natural (Criteria.Classes.Length);
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

    procedure Init (Criteria : in out Criterion_Class;
                    Y : ML_Types.List_Of_Value_Data_Lists;
                    Sample_Weight : Classifier_Types.Weight_List;
                    Num_Weighted_Samples, Start, Stop : Natural;
                    Sample_Indices : Classifier_Types.Natural_List) is
        Y_I             : ML_Types.Value_Data_List;
        Sum_Total_Index : Natural := 1;
        i               : Natural;
        w               : Float := 1.0;
        c               : ML_Types.Value_Record;
    begin
        Criteria.Y := Y;
        Criteria.Sample_Weight := Sample_Weight;
        Criteria.Num_Weighted_Samples := Num_Weighted_Samples;
        Criteria.Sample_Indices := Sample_Indices;
        Criteria.Start := Start;
        Criteria.Stop := Stop;
        Criteria.Sample_Indices := Sample_Indices;

        Criteria.Num_Node_Samples := Stop - Start;
        Criteria.Num_Weighted_Node_Samples := 0;
        Criteria.Pos := Criteria.Start;
        Criteria.Num_Weighted_Left := 0;
        Criteria.Num_Weighted_Right := Criteria.Num_Weighted_Node_Samples;

        for k in 1 .. Criteria.Num_Outputs loop
            for index in 1 .. Criteria.Classes.Length loop
                Criteria.Sum_Total.Replace_Element (Sum_Total_Index, 0.0);
                Sum_Total_Index := Sum_Total_Index + Criteria.Stride;
            end loop;
        end loop;

        Criteria.Weighted_Node_Samples := 0.0;
        for p in Start .. Stop loop
            i := Criteria.Sample_Indices.Element (p);
            Y_I := Y.Element (i);
            if not Sample_Weight.Is_Empty then
                w := Sample_Weight.Element (i);
            end if;

            for k in 1 .. Criteria.Num_Outputs loop
                c := Y_I.Element (k);
                Sum_Total_Index := (k - 1) * Criteria.Stride + c.Integer_Value;
                Criteria.Sum_Total.Replace_Element
                   (Sum_Total_Index,
                   Criteria.Sum_Total.Element (Sum_Total_Index) + w);
            end loop;

            Criteria.Weighted_Node_Samples :=
              Criteria.Weighted_Node_Samples + w;
        end loop;

        Reset (Criteria);

    end Init;

    --  ------------------------------------------------------------------------

    procedure Reset (Criteria : in out Criterion_Class) is
        Sum_Total_Index : Natural := 1;
        Sum_Left_Index  : Natural := 1;
        Sum_Right_Index : Natural := 1;
    begin
        Criteria.Pos := Criteria.Start;
        Criteria.Num_Weighted_Left := 0;
        Criteria.Num_Weighted_Right := Criteria.Num_Weighted_Node_Samples;

        for k in 1 .. Criteria.Num_Outputs loop
            for index in 1 .. Criteria.Classes.Length loop
                Criteria.Sum_Left.Replace_Element (Sum_Left_Index, 0.0);
                Criteria.Sum_Right.Replace_Element
                  (Sum_Right_Index, Criteria.Sum_Total.Element (Sum_Total_Index));
            end loop;

            Sum_Total_Index := Sum_Total_Index + Criteria.Stride;
            Sum_Left_Index := Sum_Left_Index + Criteria.Stride;
            Sum_Right_Index := Sum_Right_Index + Criteria.Stride;
        end loop;

    end Reset;

    --  ------------------------------------------------------------------------

    procedure Update (Criteria : in out Criterion_Class) is
    begin
        null;
    end Update;

    --  ------------------------------------------------------------------------

end Criterion;
