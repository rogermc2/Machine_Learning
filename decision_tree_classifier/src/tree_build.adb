
--  Based on scikit-learn/sklearn/tree _tree.pxd class TreeBuilder
--  The TreeBuilder recursively builds a Tree object from training samples,
--  using a Splitter object for splitting internal nodes and assigning
--  values to leaves.
--  Tree_Builder controls the various stopping criteria and the node splitting
--  evaluation order, e.g. depth-first or best-first.

package body Tree_Build is

    --  ------------------------------------------------------------------------

    procedure Build_Best_First_Tree
      (aBuilder : in out Tree_Builder; X, Y : ML_Types.List_Of_Value_Data_Lists;
       Sample_Weight : Classifier_Types.Weight_List;
       theTree : in out Tree.Tree_Data) is
    begin
        null;
    end Build_Best_First_Tree;

    --  ------------------------------------------------------------------------

    procedure Build_Depth_First_Tree
      (aBuilder : in out Tree_Builder; X, Y : ML_Types.List_Of_Value_Data_Lists;
       Sample_Weight : Classifier_Types.Weight_List;
       theTree : in out Tree.Tree_Data) is
    begin
        null;
    end Build_Depth_First_Tree;

    --  ------------------------------------------------------------------------

end Tree_Build;
