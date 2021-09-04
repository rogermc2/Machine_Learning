--  Based on scikit-learn/sklearn/tree _splitter.pyx class BestSplitter

with Maths;

package body Node_Splitter is

    procedure Reset_Node (Self : in out Split_Class; Start, Stop : Natural;
                          Weighted_Node_Samples : in out Classifier_Types.Weight_List) is
    begin
        Self.Start := Start;
        Self.Stop := Stop;
        if Weighted_Node_Samples.Is_Empty then
            Weighted_Node_Samples.Append
              (Float (Self.Criteria.Num_Weighted_Node_Samples));
        else
            Weighted_Node_Samples.Replace_Element
              (1, Float (Self.Criteria.Num_Weighted_Node_Samples));
        end if;
    end Reset_Node;

    --  -------------------------------------------------------------------------

    procedure Split_Node (Self : Split_Class; Impurity : Float;
                          theSplit : Split_Record;
                          Num_Constant_Features : ML_Types.Value_Data_List) is
        use Maths.Float_Math_Functions;
        Features             : ML_Types.List_Of_Value_Data_Lists :=
                                 Self.Features;
        Current              : Split_Record;
        Num_Features         : Natural := Natural (Features.Length);
        Max_Features         : Natural := Self.Max_Features;
        Num_Known_Constants  : Natural :=
                                 Num_Constant_Features.Element (1).Integer_Value;
        Num_Total_Constants  : Natural := Num_Known_Constants;
        Num_Visited_Features : Natural := 0;
        Num_Found_Constants  : Natural := 0;
        Num_Drawn_Constants  : Natural := 0;
        Samples              : ML_Types.Rows_Vector := Self.Samples;
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
                Current.Feature := Features.Element (F_Index);
            end if;
        end loop;

    end Split_Node;

end Node_Splitter;
