
with ML_Types;
with Classifier_Types;

package Criterion is

    type Criterion_Class is record
        Y                         : ML_Types.List_Of_Value_Data_Lists;
        Classes                   : ML_Types.Value_Data_List;
        Sample_Indices            : Classifier_Types.Natural_List;
        Num_Outputs               : Natural := 0;
        Num_Weighted_Node_Samples : Natural := 0;
        Num_Node_Samples          : Natural := 0;
        Num_Weighted_Left         : Natural := 0;
        Num_Weighted_Right        : Natural := 0;
        Num_Weighted_Samples      : Natural := 0;
        Sample_Weight             : Classifier_Types.Weight_List;
        Weighted_Node_Samples     : Float := 0.0;
        --  For classification criteria, Sum_Total is the sum of the weighted
        --  count of each label.
        --  For regression, Sum_Total is the sum of w*y.
        --  Sum_Total [k] is equal to
        --  sum_{i=start}^{end-1} w[samples[i]]*y[samples[i], k]
        --  where k is the output index.
        Sum_Total                 : Classifier_Types.Weight_List;
        Sum_Left                  : Classifier_Types.Weight_List;
        Sum_Right                 : Classifier_Types.Weight_List;
        Stride                    : Natural := 0;
        Start                     : Natural := 0;
        Pos                       : Natural := 0;
        Stop                      : Natural := 0;
    end record;

    function Gini_Node_Impurity (Criteria : in out Criterion_Class)
                                 return Float;
    procedure Init (Criteria : in out Criterion_Class;
                    Y : ML_Types.List_Of_Value_Data_Lists;
                    Sample_Weight : Classifier_Types.Weight_List;
                    Num_Weighted_Samples, Start, Stop : Natural;
                    Sample_Indices : Classifier_Types.Natural_List);
    procedure Reset (Criteria : in out Criterion_Class);
    procedure Update (Criteria : in out Criterion_Class);

end Criterion;
