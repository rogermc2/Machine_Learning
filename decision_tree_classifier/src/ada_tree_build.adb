--  Based on scikit-learn/sklearn/tree _tree.pyx class DepthFirstTreeBuilder

--  with Ada.Text_IO; use Ada.Text_IO;

with Node_Splitter;
with Tree;
with Tree_Build;

package body Ada_Tree_Build is

   Epsilon : constant Float := 10.0 ** (-10);

   First          : Boolean := True;
   Max_Depth_Seen : Natural := 0;

   procedure Init_Tree_Builder
     (Builder               : in out Tree_Builder;
      Splitter              : Node_Splitter.Splitter_Class;
      Min_Samples_Split     : Natural := 0;
      Min_Samples_Leaf      : Natural := 0;
      Min_Weight_Leaf       : Float := 0.0;
      Max_Depth             : Natural := 0;
      Min_Impurity_Decrease : Float := 0.0);

   --  ------------------------------------------------------------------
   --  Based on scikit-learn/sklearn/tree _tree.pyx DepthFirstTreeBuilder.build
   procedure Add_Branch
     (theTree               : in out Tree.Tree_Class;
      Builder               : in out Tree_Builder;
      Start, Stop           : in out Natural;
      Num_Constant_Features : in out Natural;
      Parent_Cursor         : in out Tree.Tree_Cursor) is
      --  Parent_Cursor is a cursor to an existing node which is the head
      --  of this branch
      use Node_Splitter;
      use Tree;
      use Nodes_Package;
      Splitter              : Node_Splitter.Splitter_Class :=
                                Builder.Splitter;
      Split                 : Split_Record;
      --  Parent_Node corresponds to popped stack_record?
      --  L199
      Parent_Node           : constant Tree.Tree_Node := Element (Parent_Cursor);
      Num_Node_Samples      : constant Natural := Stop - Start;  --  L207
      Is_Leaf               : Boolean := False;
      Impurity              : Float := Float'Last;
      Weighted_Node_Samples : Float := 0.0;
      Depth                 : Natural := Parent_Node.Depth;
      Child_Cursor          : Tree.Tree_Cursor;
   begin
      --  L208
      --  Reset_Node resets splitter to use samples (Start .. Stop)
      Reset_Node (Splitter, Start, Stop, Weighted_Node_Samples);
      if First then
         Impurity := Node_Impurity (Splitter);
         First := False;
      end if;

      --  L210
      Is_Leaf := Depth >= Builder.Max_Depth or
        Num_Node_Samples < Builder.Min_Samples_Split or
        Num_Node_Samples < 2 * Builder.Min_Samples_Leaf or
        Weighted_Node_Samples < 2.0 * Builder.Min_Weight_Leaf or
        --  if Impurity == 0.0 with tolerance for rounding errors
        Impurity <= Epsilon;

      --  L222
      if not Is_Leaf then
         Split := Split_Node (Splitter, Impurity, Num_Constant_Features);
         Is_Leaf := Split.Pos_I >= Stop or
           Split.Improvement + Epsilon <= Builder.Min_Impurity_Decrease;
      end if;
      --  L229
      Child_Cursor := Tree_Build.Add_Node
        (theTree, Splitter, Depth, Parent_Cursor, True, Is_Leaf, Split.Feature_Index,
         Impurity, Split.Threshold, Weighted_Node_Samples);

      --  L241 Nodes already added by Tree_Build.Add_Node

      --  L254
      if Depth > Max_Depth_Seen then
         Max_Depth_Seen := Depth;
      end if;

      if not Is_Leaf then
         Add_Branch (theTree, Builder, Start, Stop, Num_Constant_Features,
                     Child_Cursor);
      end if;

   end Add_Branch;

   --  ------------------------------------------------------------------

   procedure Build_Tree
     (theTree       : in out Tree.Tree_Class;
      X             : ML_Types.List_Of_Value_Data_Lists;
      Y             : Classifier_Types.List_Of_Natural_Lists;
      Sample_Weight : Weights.Weight_List) is
      use Tree.Nodes_Package;
      use Node_Splitter;
      Builder               : Tree_Builder;
      Splitter              : Splitter_Class;
      Num_Constant_Features : Natural := 0;
      Start                 : Positive := 1;
      Stop                  : Positive := X.Element (1).Last_Index;
      Top_Node              : Tree.Tree_Node;
      Top_Node_Cursor       : Cursor;
   begin
      --  L163
      Node_Splitter.Init (Splitter, X, Y, Sample_Weight);
      Init_Tree_Builder (Builder, Splitter);

      Top_Node.Impurity := Splitter.Node_Impurity;
      theTree.Nodes.Prepend_Child (theTree.Nodes.Root, Top_Node);
      Top_Node_Cursor := Last_Child (theTree.Nodes.Root);
      Add_Branch (theTree, Builder, Start, Stop,
                  Num_Constant_Features, Top_Node_Cursor);

   end Build_Tree;

   --  ------------------------------------------------------------------

   procedure Init_Tree_Builder
     (Builder               : in out Tree_Builder;
      Splitter              : Node_Splitter.Splitter_Class;
      Min_Samples_Split     : Natural := 0;
      Min_Samples_Leaf      : Natural := 0;
      Min_Weight_Leaf       : Float := 0.0;
      Max_Depth             : Natural := 0; --  -1 means undefined
      Min_Impurity_Decrease : Float := 0.0) is

   begin
      Builder.Splitter := Splitter;
      Builder.Min_Samples_Split := Min_Samples_Split;
      Builder.Min_Samples_Leaf := Min_Samples_Leaf;
      Builder.Min_Weight_Leaf := Min_Weight_Leaf;
      Builder.Max_Depth := Max_Depth;
      Builder.Min_Impurity_Decrease := Min_Impurity_Decrease;
   end Init_Tree_Builder;

   --  ------------------------------------------------------------------

end Ada_Tree_Build;
