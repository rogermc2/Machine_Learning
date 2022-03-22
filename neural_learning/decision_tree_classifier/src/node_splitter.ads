
--  Based on scikit-learn/sklearn/tree _splitter.pxd class Splitter

with NL_Types; use NL_Types;
with Criterion;
with Tree;
with Weights;

package Node_Splitter is

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

   type Splitter_Class is record
      Criteria             : Criterion.Criterion_Class;
      Max_Features         : Tree.Index_Range := 1;  --  Number of features to test
      Min_Leaf_Samples     : Natural := 0;
      Min_Leaf_Weight      : Float := 0.0;
      --  Samples:
      Sample_Indices       : Natural_List;
      Feature_Indices      : Natural_List;
      Constant_Features_I  : Natural_List;
      Num_Classes          : Natural;
      Feature_Values       : Float_List;
      Num_Samples          : Natural := 0;
      Weighted_Samples     : Float := 0.0;
      --  encoded version of sample Y
      Y_Encoded            : Natural_List;
      Sample_Weight        : Weights.Weight_List;
      Node_Impurity        : Float := -Float'Last;
      Start_Row            : Positive := 1;
      Stop_Row             : Positive := 1;
      --  BaseDenseSplitter elements
      X                    : Float_List_2D;
      Total_Samples        : Natural := 0;
   end record;

   Node_Splitter_Error : Exception;

   procedure C_Init (Self             : in out Splitter_Class;
                     Max_Features     : Tree.Index_Range := 1;
                     Min_Leaf_Samples : Integer := 0;
                     Min_Leaf_Weight  : Float := 0.0);
   procedure Initialize_Splitter
     (Self             : in out Splitter_Class;
      Input_X          : Float_List_2D;
      Y_Encoded        : Natural_List;
      Sample_Weight    : Weights.Weight_List;
      Min_Leaf_Samples : Positive := 1);
   function Entropy_Node_Impurity (Self : Splitter_Class) return Float;
   function Gini_Node_Impurity (Self : Splitter_Class) return Float;
   procedure Node_Value (Self   : Splitter_Class;
                         Values : out Weights.Weight_List);
   procedure Reset_Node
     (Splitter              : in out Splitter_Class;
      Start_Row, Stop_Row   : Positive;
      Weighted_Node_Samples : in out Float);
   function Split_Node (Self                  : in out Splitter_Class;
                        Impurity              : Float;
                        Num_Constant_Features : in out Natural)
                        return Split_Record;

end Node_Splitter;
