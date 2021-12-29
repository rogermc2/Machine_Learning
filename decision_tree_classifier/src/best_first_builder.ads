
with Classifier_Types;
with ML_Types;
with Node_Splitter;
with Tree;
with Weights;

package Best_First_Builder is

    type Tree_Builder is record
        Splitter              : Node_Splitter.Splitter_Class;
        --  Minimum samples in an internal node
        Min_Samples_Split     : Positive := 2;
        Min_Samples_Leaf      : Positive := 1;
        Min_Weight_Leaf       : Float := 0.0;
        Max_Depth             : Integer := -1;
        Min_Impurity_Decrease : Float := 0.0;
    end record;

    Best_First_Build_Error : Exception;

    procedure Build_Tree
      (theTree               : in out Tree.Tree_Class;
       Splitter              : in out Node_Splitter.Splitter_Class;
       X                     : ML_Types.Value_Data_Lists_2D;
       Y_Encoded             : Classifier_Types.Natural_Lists_2D;
       Sample_Weights        : Weights.Weight_List);

end Best_First_Builder;
