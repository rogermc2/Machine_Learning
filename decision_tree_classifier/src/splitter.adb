
--  Based on scikit-learn/sklearn/tree _splitter.pyx class BestSplitter

package body Splitter is

    procedure Split_Node (Self : Split_Class; Impurity : Float;
                          theSplit : Split_Record;
                          Num_Constant_Features : ML_Types.Value_Data_List) is
        Num_Features         : Natural := Self.Num_Features;
        Max_Features         : Natural := Self.Max_Features;
        Num_Known_Constants  : Natural :=
                                 Num_Constant_Features.Element (1).Integer_Value;
        Num_Total_Constants  : Natural := Num_Known_Constants;
        Num_Visited_Features : Natural := 0;
        Num_Found_Constants  : Natural := 0;
        Num_Drawn_Constants  : Natural := 0;
        Samples              : ML_Types.Rows_Vector := Self.Samples;
        F_Index              : Natural := Num_Features;
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
            null;
        end loop;

    end Split_Node;

end Splitter;
