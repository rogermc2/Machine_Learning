
--  Based on scikit-learn/sklearn/tree _tree.pxd class TreeBuilder
--  The TreeBuilder recursively builds a Tree object from training samples,
--  using a Splitter object for splitting internal nodes and assigning
--  values to leaves.
--  Tree_Builder controls the various stopping criteria and the node splitting
--  evaluation order, e.g. depth-first or best-first.

with ML_Types;
with Classifier_Types;
with Tree;

package Tree_Build is

    type Type_Of_Tree is (Depth_First_Tree, Best_First_Tree);

    type Tree_Builder (Tree_Kind : Type_Of_Tree := Depth_First_Tree) is record
    --  Minimum samples in an internal node
        Min_Samples_Split     : Natural := 0;
        Min_Samples_Leaf      : Natural := 0;
        Min_Weight_Leaf       : Natural := 0;
        Max_Depth             : Natural := 0;
        Min_Impurity_Decrease : Natural := 0;
    end record;

    procedure Build_Best_First_Tree
      (aBuilder : in out Tree_Builder; X, Y : ML_Types.List_Of_Value_Data_Lists;
       Sample_Weight : Classifier_Types.Weight_List;
       theTree : in out Tree.Tree_Data);
    procedure Build_Depth_First_Tree
      (aBuilder : in out Tree_Builder; X, Y : ML_Types.List_Of_Value_Data_Lists;
       Sample_Weight : Classifier_Types.Weight_List;
       theTree : in out Tree.Tree_Data);

end Tree_Build;
