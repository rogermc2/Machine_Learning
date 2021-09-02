
--  Based on scikit-learn/sklearn/tree _tree.pxd class TreeBuilder
--  The TreeBuilder recursively builds a Tree object from training samples,
--  using a Splitter object for splitting internal nodes and assigning
--  values to leaves.
--  Tree_Builder controls the various stopping criteria and the node splitting
--  evaluation order, e.g. depth-first or best-first.

with ML_Types; use ML_Types;
with Tree;

package Tree_Build is

    type Tree_Builder is record
        --  Minimum samples in an internal node
        Min_Samples_Split     : Natural := 0;
        Min_Samples_Leaf      : Natural := 0;
        Min_Weight_Leaf       : Natural := 0;
        Max_Depth             : Natural := 0;
        Min_Impurity_Decrease : Natural := 0;
    end record;

    function Build (aBuilder : Tree_Builder; theTree : Tree.Tree_Data;
                     Rows : in out Rows_Vector) return Tree_Type;

end Tree_Build;
