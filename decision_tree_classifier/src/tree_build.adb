
--  Based on scikit-learn/sklearn/tree _tree.pxd class TreeBuilder
--  The TreeBuilder recursively builds a Tree object from training samples,
--  using a Splitter object for splitting internal nodes and assigning
--  values to leaves.
--  Tree_Builder controls the various stopping criteria and the node splitting
--  evaluation order, e.g. depth-first or best-first.

with Builder;

package body Tree_Build is

    function Build (aBuilder : Tree_Builder; theTree : Tree.Tree_Data;
                    Rows : in out Rows_Vector) return Tree_Type is
    begin
        return Builder.Build_Tree (Rows);
    end Build;

end Tree_Build;
