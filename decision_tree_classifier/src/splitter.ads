
with ML_Types;
with Classifier_Types;

package Splitter is

    type Split_Record is record
        Threshold         : Natural := 0;
        Improvement       : Natural := 0;
        Improvement_Left  : Natural := 0;
        Improvement_Right : Natural := 0;
    end record;

    type Split_Class is record
        Max_Features         : Natural := 0;  --  Number of features to test
        Min_Leaf_Samples     : Natural := 0;
        Min_Leaf_Weight      : Natural := 0;
        Samples              : ML_Types.Rows_Vector;
        Num_Weighted_Samples : Natural := 0;
        Sample_Weight        : Classifier_Types.Weight_List;
    end record;

    procedure Split_Node (Self : Split_Class; Impurity : Float;
                          theSplit : Split_Class);

end Splitter;
