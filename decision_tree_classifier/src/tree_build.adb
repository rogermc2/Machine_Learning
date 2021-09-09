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

   function Add_Node (Self                  : in out Tree.Tree_Class;
                      Parent                : ML_Types.Tree_Node_Type;
                      Is_Left, Is_Leaf      : Boolean;
                      Feature               : Positive;
                      Impurity, Threshold   : Float;
                      Node_Samples          : Positive;
                      Weighted_Node_Samples : Float) return Natural is

      use Tree;
      Node_ID  : Index_Range := Index_Range (Self.Node_Count);
   begin
      if Node_ID >= Self.Capacity then
         null;
      end if;

      return Natural (Node_ID);

   end Add_Node;

   --  ------------------------------------------------------------------------

   procedure Add_Split_Node
     (Self              : in out Tree_Builder;
      Splitter          : in out Node_Splitter.Splitter_Class;
      aTree             : in out Tree.Tree_Class;
      Start, Stop       : Positive; Impurity : in out Float;
      Is_First, Is_Left : Boolean;
      Parent            : ML_Types.Tree_Node_Type; Depth : Positive) is

      Node_ID           : Natural;
      Node_Samples      : Positive := Stop - Start;
      Node_Val          : Float;
      Is_Leaf           : Boolean;
      aSplit            : Node_Splitter.Split_Record;
      Constant_Features : ML_Types.Value_Data_List;
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
         aSplit := Node_Splitter.Split_Node (Splitter, Impurity,
                                             Constant_Features);
         Is_Leaf := aSplit.Pos >= Stop or
           aSplit.Improvement + Epsilon < Self.Min_Impurity_Decrease;
      end if;

      Node_ID := Add_Node (aTree, Parent, Is_Left, Is_Leaf, aSplit.Feature_Index,
                           aSplit.Threshold, Impurity, Node_Samples,
                           Splitter.Weighted_Samples);
      Node_Splitter.Node_Value (Splitter, Node_Val);
      aTree.Values (1, 1, 1) := Node_Val;
   end Add_Split_Node;

   --  ------------------------------------------------------------------------

   procedure Build_Tree
     (aBuilder      : in out Tree_Builder;
      theTree       : in out Tree.Tree_Class;
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
     (aBuilder      : in out Tree_Builder;
      theTree       : in out Tree.Tree_Class;
      X, Y          : ML_Types.List_Of_Value_Data_Lists;
      Sample_Weight : Classifier_Types.Weight_List) is
   begin
      null;
   end Build_Best_First_Tree;

   --  ------------------------------------------------------------------------

   procedure Build_Depth_First_Tree
     (aBuilder      : in out Tree_Builder;
      theTree       : in out Tree.Tree_Class;
      X, Y          : ML_Types.List_Of_Value_Data_Lists;
      Sample_Weight : Classifier_Types.Weight_List) is
      Start         : Positive;
      Stop          : Positive;
      Is_First      : Boolean;
      Is_Left       : Boolean;
      Parent        : ML_Types.Tree_Node_Type;
      Impurity      : Float;
      Depth         : Positive;
      Node_Samples  : Positive;
      Is_Leaf       : Boolean;
      aSplitter     : Node_Splitter.Splitter_Class;
   begin
      Add_Split_Node (aBuilder, aSplitter, theTree, Start, Stop, Impurity,
                      Is_First, Is_Left, Parent, Depth);
   end Build_Depth_First_Tree;

   --  ------------------------------------------------------------------------

end Tree_Build;
