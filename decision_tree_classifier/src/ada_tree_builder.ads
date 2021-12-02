
with Classifier_Types;
with ML_Types;
with Node_Splitter;
with Tree;
with Weights;

package Ada_Tree_Builder is

    type Tree_Builder is record
        Splitter              : Node_Splitter.Splitter_Class;
        --  Minimum samples in an internal node
        Min_Samples_Split     : Positive := 2;
        Min_Samples_Leaf      : Positive := 1;
        Min_Weight_Leaf       : Float := 0.0;
        Max_Depth             : Integer := -1;
        Min_Impurity_Decrease : Float := 0.0;
    end record;

    Ada_Tree_Build_Error : Exception;

    procedure Build_Tree
      (theTree               : in out Tree.Tree_Class;
       Splitter              : in out Node_Splitter.Splitter_Class;
       X                     : ML_Types.Value_Data_Lists_2D;
       Y_Encoded             : Classifier_Types.Natural_Lists_2D;
       Sample_Weights        : Weights.Weight_List;
       Min_Samples_Split     : Positive;
       Min_Samples_Leaf      : Positive;
       Min_Weight_Leaf       : Float;
       Max_Depth             : Integer;
       Min_Impurity_Decrease : Float);

end Ada_Tree_Builder;
