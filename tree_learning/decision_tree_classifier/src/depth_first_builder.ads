
with NL_Types;
with Node_Splitter;
with Tree;
with Tree_Build;

package Depth_First_Builder is

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

    procedure Build_Tree (theTree   : in out Tree.Tree_Class;
                          Builder   : in out Tree_Build.Tree_Builder;
                          Y_Encoded : NL_Types.Natural_Lists_2D);

end Depth_First_Builder;
