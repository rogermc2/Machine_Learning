
--  Based on scikit-learn/sklearn/tree _splitter.pxd class Splitter

with Ada.Strings.Unbounded;
with ML_Types;
with Classifier_Types;
with Criterion;

package Node_Splitter is
   use Ada.Strings.Unbounded;

   type Split_Record is record
      Feature_Index  : Positive := 1;
      Start_Index    : Positive := 1;  --  Left start?
      End_Index      : Positive := 1;
      Pos_I          : Positive := 1;  --  Right start?
      Threshold      : Float;
      Improvement    : Float := 0.0;
      Impurity_Left  : Float;
      Impurity_Right : Float;
   end record;

   type Splitter_Class is record
      Criteria             : Criterion.Criterion_Class;
      Max_Features         : Natural := 0;  --  Number of features to test
      Min_Leaf_Samples     : Natural := 0;
      Min_Leaf_Weight      : Float := 0.0;
      Sample_Indices       : Classifier_Types.Natural_List;
      X_Samples            : ML_Types.List_Of_Value_Data_Lists;
      Y_Samples            : ML_Types.List_Of_Value_Data_Lists;
      Feature_Indices      : Classifier_Types.Natural_List;
      Constant_Features_I  : Classifier_Types.Natural_List;
      Feature_Values       : ML_Types.Value_Data_List;
      Num_Samples          : Natural := 0;
      Weighted_Samples     : Float := 0.0;
      Target_Y             : ML_Types.List_Of_Value_Data_Lists;
      Sample_Weight        : Classifier_Types.Weight_List;
      Node_Impurity        : Float;
      Start_Index          : Positive := 1;
      End_Index            : Positive := 1;
      --  BaseDenseSplitter elements
      Input_X              : ML_Types.List_Of_Value_Data_Lists;
      Total_Samples        : Natural := 0;
   end record;

   Node_Splitter_Error : Exception;

   procedure Init (Self               : in out Splitter_Class;
                   Input_X, Target_Y  : ML_Types.List_Of_Value_Data_Lists;
                   Sample_Weight      : Classifier_Types.Weight_List);
   procedure Node_Value (Self : Splitter_Class; Value : out Float);
   procedure Reset_Node
     (Split                 : in out Splitter_Class;
      Weighted_Node_Samples : in out Float);
   function Split_Node (Self                  : in out Splitter_Class;
                        Impurity              : Float;
                        Num_Constant_Features : in out Natural)
                        return Split_Record;

end Node_Splitter;
