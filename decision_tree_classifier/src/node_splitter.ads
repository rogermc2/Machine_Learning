
--  Based on scikit-learn/sklearn/tree _splitter.pxd class Splitter

with ML_Types;
with Classifier_Types;
with Criterion;

package Node_Splitter is

   type Split_Record is record
      Feature_Index  : Natural := 0;
      Pos            : Positive := 1;
      Threshold      : Float;
      Improvement    : Float;
      Impurity_Left  : Float;
      Impurity_Right : Float;
   end record;

   type Splitter_Class is record
      Criteria             : Criterion.Criterion_Class;
      Max_Features         : Natural := 0;  --  Number of features to test
      Min_Leaf_Samples     : Natural := 0;
      Min_Leaf_Weight      : Float := 0.0;
      Sample_Indices       : Classifier_Types.Natural_List;
      Feature_Indices      : Classifier_Types.Natural_List;
      Constant_Features    : Classifier_Types.Natural_List;
      Feature_Values       : ML_Types.Value_Data_List;
      Num_Samples          : Natural := 0;
      Weighted_Samples     : Float := 0.0;
      Start                : Natural := 0;
      Stop                 : Natural := 0;
      Y                    : ML_Types.List_Of_Value_Data_Lists;
      Sample_Weight        : Classifier_Types.Weight_List;
      Node_Impurity        : Float;
      --  BaseDenseSplitter elements
      X                    : ML_Types.List_Of_Value_Data_Lists;
      Total_Samples        : Natural := 0;
   end record;

   procedure Init (Self          : in out Splitter_Class;
                   X, Y          : ML_Types.List_Of_Value_Data_Lists;
                   Sample_Weight : Classifier_Types.Weight_List);
   procedure Reset_Node
     (Split                 : in out Splitter_Class; Start, Stop : Natural;
      Weighted_Node_Samples : in out Float);
   function Split_Node (Self              : in out Splitter_Class;
                        Constant_Features : in out ML_Types.Value_Data_List)
                         return Split_Record;

end Node_Splitter;
