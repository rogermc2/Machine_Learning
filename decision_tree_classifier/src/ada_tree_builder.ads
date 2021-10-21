
with Classifier_Types;
with Node_Splitter;
with Tree;

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
      (theTree       : in out Tree.Tree_Class;
       Splitter      : in out Node_Splitter.Splitter_Class;
       Y_Encoded     : Classifier_Types.List_Of_Natural_Lists;
       Max_Depth     : Integer);

end Ada_Tree_Builder;
