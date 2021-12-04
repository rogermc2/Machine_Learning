
--  Based on scikit-learn/sklearn/tree _splitter.pxd class Splitter

with Ada.Strings.Unbounded;
with ML_Types;
with Classifier_Types;
with Criterion;
with Tree;
with Weights;

package Node_Splitter is
    use Ada.Strings.Unbounded;

    type Split_Record is record
        Feature        : Positive := 1;
        --  Split_Row is the count of samples below threshold for Feature
        --  >= end if the node is a leaf.
        Split_Row      : Positive := 2;  --  Right start?
        Threshold      : Float := 0.0;
        Improvement    : Float := -Float'Last;
        Impurity_Left  : Float := -Float'Last;
        Impurity_Right : Float := -Float'Last;
    end record;

    use Classifier_Types;
    type Splitter_Class is record
        Criteria             : Criterion.Criterion_Class;
        Max_Features         : Tree.Index_Range := 1;  --  Number of features to test
        Min_Leaf_Samples     : Natural := 0;
        Min_Leaf_Weight      : Float := 0.0;
        --  Samples:
        Sample_Indices       : Natural_List := Natural_Package.Empty_Vector;
        Feature_Indices      : Natural_List := Natural_Package.Empty_Vector;
        Constant_Features_I  : Natural_List := Natural_Package.Empty_Vector;
        Num_Classes          : Natural_List := Natural_Package.Empty_Vector;
        Feature_Values       : ML_Types.Value_Data_List :=
                                 ML_Types.Value_Data_Package.Empty_Vector;
        Num_Samples          : Natural := 0;
        Weighted_Samples     : Float := 0.0;
        --  encoded version of sample Y
        Y                    : Natural_Lists_2D :=
                                 Natural_List_Package.Empty_Vector;
        Sample_Weight        : Weights.Weight_List :=
                                 Classifier_Types.Float_Package.Empty_Vector;
        Node_Impurity        : Float := -Float'Last;
        Start_Row            : Positive := 1;
        Stop_Row             : Positive := 1;
        --  BaseDenseSplitter elements
        X                    : ML_Types.Value_Data_Lists_2D :=
                                 ML_Types.Value_Lists_Data_Package.Empty_Vector;
        Total_Samples        : Natural := 0;
    end record;

    Node_Splitter_Error : Exception;

    procedure C_Init (Self          : in out Splitter_Class;
                      Max_Features  : Tree.Index_Range := 1;
                      Min_Leaf_Samples : Integer := 0;
                      Min_Leaf_Weight : Float := 0.0);
    procedure Init (Self             : in out Splitter_Class;
                    Input_X          : ML_Types.Value_Data_Lists_2D;
                    Y_Encoded        : Classifier_Types.Natural_Lists_2D;
                    Sample_Weight    : Weights.Weight_List;
                    Min_Leaf_Samples : Positive := 1);
    function Entropy_Node_Impurity (Self : Splitter_Class) return Float;
    function Gini_Node_Impurity (Self : Splitter_Class) return Float;
    procedure Node_Value (Self   : Splitter_Class;
                          Values : out Weights.Weight_Lists_2D);
    procedure Reset_Node
      (Splitter              : in out Splitter_Class;
       Start_Row, Stop_Row   : Positive;
       Weighted_Node_Samples : in out Float);
    function Split_Node (Self                  : in out Splitter_Class;
                         Impurity              : Float;
                         Num_Constant_Features : in out Natural)
                         return Split_Record;

end Node_Splitter;
