
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
      Split_Row      : Positive := 2;  --  Right start?
      Threshold      : Float;
      Improvement    : Float := -Float'Last;
      Impurity_Left  : Float := -Float'Last;
      Impurity_Right : Float := -Float'Last;
   end record;

   type Splitter_Class is record
      Criteria             : Criterion.Criterion_Class;
      Max_Features         : Tree.Index_Range := 1;  --  Number of features to test
      Min_Leaf_Samples     : Natural := 0;
      Min_Leaf_Weight      : Float := 0.0;
      Sample_Indices       : Classifier_Types.Natural_List;  --  Samples
      Feature_Indices      : Classifier_Types.Natural_List;
      Constant_Features_I  : Classifier_Types.Natural_List;
      Num_Classes          : Classifier_Types.Natural_List;
      Feature_Values       : ML_Types.Value_Data_List;
      Num_Samples          : Natural := 0;
      Weighted_Samples     : Float := 0.0;
      --  encoded version of sample Y
      Y                    : Classifier_Types.List_Of_Natural_Lists;
      Sample_Weight        : Weights.Weight_List;
      Node_Impurity        : Float;
      Start_Row            : Positive := 1;
      End_Row              : Positive := 1;
      --  BaseDenseSplitter elements
      X                    : ML_Types.Value_Data_Lists_2D;
      Total_Samples        : Natural := 0;
   end record;

   Node_Splitter_Error : Exception;

    procedure C_Init (Self          : in out Splitter_Class;
                      Criteria      : Criterion.Criterion_Class;
                      Max_Features  : Tree.Index_Range := 1;
                      Min_Leaf_Samples : Positive := 1;
                      Min_Leaf_Weight : Float := 0.0);
   procedure Init (Self             : in out Splitter_Class;
                   Input_X          : ML_Types.Value_Data_Lists_2D;
                   Y_Encoded        : Classifier_Types.List_Of_Natural_Lists;
                   Sample_Weight    : Weights.Weight_List;
                   Min_Leaf_Samples : Positive := 1);
   function Node_Impurity (Self : Splitter_Class) return Float;
   procedure Node_Value (Self   : Splitter_Class;
                         Values : out Weights.Weight_Lists_List);
   procedure Reset_Node
     (Splitter              : in out Splitter_Class;
      Start_Row, End_Row    : Positive;
      Weighted_Node_Samples : in out Float);
   function Split_Node (Self                  : in out Splitter_Class;
                        Impurity              : Float;
                        Num_Constant_Features : in out Natural)
                        return Split_Record;

end Node_Splitter;
