--  Based on scikit-learn/sklearn/tree _tree.pxd class TreeBuilder
--  The TreeBuilder recursively builds a Tree object from training samples,
--  using a Splitter object for splitting internal nodes and assigning
--  values to leaves.
--  Tree_Builder controls the various stopping criteria and the node splitting
--  evaluation order, e.g. depth-first or best-first.

with ML_Types;
with Node_Splitter;
with Tree;
with Weights;

package Tree_Build is

   type Type_Of_Tree is (Depth_First_Tree, Best_First_Tree);

   type Tree_Builder (Tree_Kind : Type_Of_Tree := Depth_First_Tree) is record
      Splitter              : Node_Splitter.Splitter_Class;
      --  Minimum samples in an internal node
      Min_Samples_Split     : Natural := 0;
      Min_Samples_Leaf      : Natural := 0;
      Min_Weight_Leaf       : Float := 0.0;
      Max_Depth             : Natural := 0;
      Min_Impurity_Decrease : Float := 0.0;
      case Tree_Kind is
         when Best_First_Tree =>
            Max_Leaf_Nodes : Natural := 0;
         when Depth_First_Tree => null;
      end case;
   end record;

   Tree_Build_Error : Exception;

   function Add_Node (theTree               : in out Tree.Tree_Class;
                      Node_ID               : out Tree.Node_Index;
                      Depth                 : in out Natural;
                      Parent_Cursor         : Tree.Tree_Cursor;
                      Is_Left, Is_Leaf      : Boolean;
                      Feature_Index         : Positive;
                      Impurity, Threshold   : Float;
                      Weighted_Node_Samples : Float) return Tree.Tree_Cursor;
   procedure Build_Best_First_Tree
     (Best_Builder  : in out Tree_Builder;
      theTree       : in out Tree.Tree_Class;
      X, Y          : ML_Types.List_Of_Value_Data_Lists;
      Sample_Weight : Weights.Weight_List);
   procedure Build_Depth_First_Tree
     (Depth_Builder : in out Tree_Builder;
      theTree       : in out Tree.Tree_Class;
      X, Y          : ML_Types.List_Of_Value_Data_Lists;
      Sample_Weight : Weights.Weight_List);
   procedure Init_Best_First_Tree
     (Best_Builder          : in out Tree_Builder;
      Splitter              : Node_Splitter.Splitter_Class;
      Min_Samples_Split     : Natural := 0;
      Min_Samples_Leaf      : Natural := 0;
      Min_Weight_Leaf       : Float := 0.0;
      Max_Depth             : Natural := 0;
      Max_Leaf_Nodes        : Integer := -1;  --  -1 means undefined
      Min_Impurity_Decrease : Float := 0.0);
   procedure Init_Depth_First_Tree
     (Depth_Builder                       : in out Tree_Builder;
      Splitter                            : Node_Splitter.Splitter_Class;
      Min_Samples_Split, Min_Samples_Leaf : Natural := 0;
      Min_Weight_Leaf                     : Float := 0.0;
      Max_Depth                           : Natural := 0;
      Min_Impurity_Decrease               : Float := 0.0);

end Tree_Build;
