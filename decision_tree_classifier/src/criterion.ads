
with ML_Types;
with Classifier_Types;

package Criterion is

    type Criterion_Class is record
        Y                         : ML_Types.List_Of_Value_Data_Lists;
        Classes                   : ML_Types.Value_Data_List;
        Num_Outputs               : Natural := 0;
        Num_Weighted_Node_Samples : Natural := 0;
        Num_Weighted_Left         : Natural := 0;
        Num_Weighted_Right        : Natural := 0;
        --  For classification criteria, Sum_Total is the sum of the weighted
        --  count of each label.
        --  For regression, Sum_Total is the sum of w*y.
        --  Sum_Total [k] is equal to
        --  sum_{i=start}^{end-1} w[samples[i]]*y[samples[i], k]
        --  where k is the output index.
        Sum_Total                 : Classifier_Types.Natural_List;
        Sum_Left                  : Classifier_Types.Natural_List;
        Sum_Right                 : Classifier_Types.Natural_List;
        Sum_Stride                : Natural := 0;
    end record;

    function Gini_Node_Impurity (Criteria : in out Criterion_Class)
                                 return Float;

end Criterion;
