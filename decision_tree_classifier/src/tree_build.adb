--  Based on scikit-learn/sklearn/tree _tree.pxd class DepthFirstTreeBuilder
--  The TreeBuilder recursively builds a Tree object from training samples,
--  using a Splitter object for splitting internal nodes and assigning
--  values to leaves.
--  Tree_Builder controls the various stopping criteria and the node splitting
--  evaluation order, e.g. depth-first or best-first.

with Node_Splitter;
with Tree;

package body Tree_Build is

   Epsilon : constant Float := 10.0 ** (-10);

   --  ------------------------------------------------------------------------

   procedure Add_Split_Node
     (Self              : in out Tree_Builder;
      Splitter          : in out Node_Splitter.Splitter_Class;
      Start, Stop       : Positive; Impurity : in out Float;
      Is_First, Is_Left : Boolean;
      Parent            : ML_Types.Tree_Node_Type; Depth : Positive) is
      Node_Samples          : Positive := Stop - Start;
      Is_Leaf               : Boolean;
      aSplit                : Node_Splitter.Split_Record;
      Num_Constant_Features : Natural := 0;
   begin
      Node_Splitter.Reset_Node (Splitter, Start, Stop,
                                Splitter.Weighted_Samples);
      if Is_First then
         Impurity := Splitter.Node_Impurity;
      end if;

      Is_Leaf := (Depth >= Self.Max_Depth) or
        (Node_Samples < Self.Min_Samples_Split) or
        (Node_Samples < 2 * Self.Min_Samples_Leaf) or
        (Impurity <= Epsilon);

      if not Is_Leaf then
         Node_Splitter.Split_Node
           (Self, Impurity, aSplit, Num_Constant_Features);
      end if;

   end Add_Split_Node;

   --  ------------------------------------------------------------------------

   procedure Build_Tree
     (aBuilder      : in out Tree_Builder;
      theTree       : in out Tree.Tree_Data;
      X, Y          : ML_Types.List_Of_Value_Data_Lists;
      Sample_Weight : Classifier_Types.Weight_List) is
      Max_Split_Nodes : Natural := aBuilder.Max_Leaf_Nodes -1;
      Splitter        : Node_Splitter.Splitter_Class;
      Capacity        : Positive := Max_Split_Nodes + aBuilder.Max_Leaf_Nodes;
   begin
      Node_Splitter.Init (Splitter, X, Y, Sample_Weight);
      Tree.Resize (theTree, Capacity);

   end Build_Tree;

   --  ------------------------------------------------------------------------

   procedure Build_Best_First_Tree
     (aBuilder      : in out Tree_Builder; X, Y : ML_Types.List_Of_Value_Data_Lists;
      Sample_Weight : Classifier_Types.Weight_List;
      theTree       : in out Tree.Tree_Data) is
   begin
      null;
   end Build_Best_First_Tree;

   --  ------------------------------------------------------------------------

   procedure Build_Depth_First_Tree
     (aBuilder      : in out Tree_Builder; X, Y : ML_Types.List_Of_Value_Data_Lists;
      Sample_Weight : Classifier_Types.Weight_List;
      theTree       : in out Tree.Tree_Data) is
   begin
      null;
   end Build_Depth_First_Tree;

   --  ------------------------------------------------------------------------

end Tree_Build;
