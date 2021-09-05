
with ML_Types;

package body Criterion is

    --  ------------------------------------------------------------------------
    --  Node_Impurity evaluates the Gini criterion as the impurity of the
    --  current node
    function Gini_Node_Impurity (Criteria : in out Criterion_Class)
                                 return Float is
        Sum_Total : constant Classifier_Types.Natural_List :=
                      Criteria.Sum_Total;
        Count_K   : Float;
        Gini      : Float := 0.0;
        Sq_Count  : Float := 0.0;
    begin
        for index_k in 1 .. Criteria.Num_Outputs loop
            Sq_Count := 0.0;
            if Natural (Criteria.Classes.Length) > Criteria.Sum_Stride then
                Criteria.Sum_Stride  := Natural (Criteria.Classes.Length);
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
                Criteria.Sum_Left.Replace_Element (Sum_Left_Index, 0);
                Criteria.Sum_Right.Replace_Element
                  (Sum_Right_Index, Criteria.Sum_Total.Element (Sum_Total_Index));
            end loop;

            Sum_Total_Index := Sum_Total_Index + Criteria.Sum_Stride;
            Sum_Left_Index := Sum_Left_Index + Criteria.Sum_Stride;
            Sum_Right_Index := Sum_Right_Index + Criteria.Sum_Stride;
        end loop;

    end Reset;

    --  ------------------------------------------------------------------------

end Criterion;
