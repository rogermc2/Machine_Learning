--  Based on scikit-learn/sklearn/tree _tree.pyx L119 class DepthFirstTreeBuilder

with Ada.Assertions; use Ada.Assertions;
with Ada.Containers;
with Ada.Text_IO; use Ada.Text_IO;

with Node_Splitter;
with Tree;
with Tree_Build;
with Weights;

package body Ada_Tree_Builder is

   Epsilon : constant Float := 10.0 ** (-10);

   First          : Boolean := True;
   Max_Depth_Seen : Positive := 1;

   procedure Init_Tree_Builder
     (Builder               : in out Tree_Builder;
      Splitter              : Node_Splitter.Splitter_Class;
      Min_Samples_Split     : Positive := 2;
      Min_Samples_Leaf      : Positive := 1;
      Min_Weight_Leaf       : Float := 0.0;
      Max_Depth             : Integer := -1;
      Min_Impurity_Decrease : Float := 0.0);

   --  ------------------------------------------------------------------
   --  Based on scikit-learn/sklearn/tree _tree.pyx DepthFirstTreeBuilder.build
   procedure Add_Branch
     (theTree               : in out Tree.Tree_Class;
      Builder               : in out Tree_Builder;
      Parent_Cursor         : in out Tree.Tree_Cursor) is
      --  Parent_Cursor is a cursor to an existing node which is the head
      --  of this branch
      use Ada.Containers;
      use Node_Splitter;
      use Tree;
      use Nodes_Package;
      Split                       : Split_Record;
      --  Parent_Node corresponds to popped stack_record?
      --  L199
      Parent_Node                 : constant Tree.Tree_Node := Element (Parent_Cursor);
      Start_Row                   : constant Positive := Parent_Node.Samples_Start;
      End_Row                     : constant Positive :=
                                      Start_Row + Parent_Node.Num_Node_Samples - 1;
      Depth                       : constant Positive := Parent_Node.Depth + 1;
      Num_Constant_Features       : Natural := Parent_Node.Num_Constant_Features;
      Is_Leaf_Node                : Boolean := False;
      Impurity                    : Float := Float'Last;
      Weighted_Node_Samples       : Float := 0.0;
      Values                      : Weights.Weight_Lists_2D;
      Left_Child_Cursor           : Tree.Tree_Cursor;
      Right_Child_Cursor          : Tree.Tree_Cursor;
      Split_Row                   : Positive := End_Row;
      Node_ID                     : Positive;
   begin
      Assert (not Parent_Node.Leaf_Node,
              "Ada_Tree_Builder.Add_Branch called on a leaf node");

      --  L209
      --  Reset_Node resets splitter to use samples (Start_Row .. End_Row)
      Reset_Node (Builder.Splitter, Start_Row, End_Row, Weighted_Node_Samples);
      --  L216 Calculate Node Impurity
      if First then
         Impurity := Gini_Node_Impurity (Builder.Splitter);
         First := False;
      else
         Impurity := Parent_Node.Impurity;
      end if;
      --        Put_Line ("Ada_Tree_Builder.Add_Branch Impurity: " &
      --                    Float'Image (Impurity));
      Put_Line ("Ada_Tree_Builder.Add_Branch Parent_Node Node_ID: " &
                  Integer'Image (Parent_Node.Node_ID));

      --  L207
      Is_Leaf_Node := Depth >= Builder.Max_Depth or
        Builder.Splitter.Num_Samples = 1 or
        Parent_Node.Num_Node_Samples < Builder.Min_Samples_Split or
        Parent_Node.Num_Node_Samples < 2 * Builder.Min_Samples_Leaf or
        Weighted_Node_Samples < 2.0 * Builder.Min_Weight_Leaf or
        --  if Impurity == 0.0 with tolerance for rounding errors
      abs (Impurity) <= Epsilon;

      --        Put_Line ("Ada_Tree_Builder.Add_Branch L222 Is_Leaf_Node: " &
      --                    Boolean'Image (Is_Leaf_Node));
      --  L222
      if not Is_Leaf_Node then
         Split := Split_Node (Builder.Splitter, Impurity,
                              Num_Constant_Features);
         Split_Row := Split.Split_Row;
         Assert (Split_Row > Start_Row and Split_Row <= End_Row,
                 "Ada_Tree_Builder.Add_Branch L222, Split_Row index " &
                   Integer'Image (Split_Row) &
                   " is not in the row index range " &
                   Integer'Image (Start_Row + 1) & " .. " &
                   Integer'Image (End_Row));
         --  L233
         Is_Leaf_Node := Split_Row >= End_Row or
           Split.Improvement + Epsilon < Builder.Min_Impurity_Decrease;
           Put_Line ("Ada_Tree_Builder.Add_Branch L228 Split.Improvement: " &
                      Float'Image (Split.Improvement));

         --  tree.add_node just generates a new initialized node
         --  right and left children are added to the tree (stack) at
         --  L245 and L251 respectively
         --  L228
         Left_Child_Cursor := Tree_Build.Add_Node
           (theTree, Builder.Splitter, Depth, Parent_Cursor, True,
            Is_Leaf_Node, Split.Feature, Impurity, Split.Threshold, Start_Row,
            Split_Row - 1, Weighted_Node_Samples);
         Node_ID := Element (Left_Child_Cursor).Node_ID;
         Put_Line ("Ada_Tree_Builder.Add_Branch left Node_ID: " &
                     Integer'Image (Node_ID));
         --  238
         Node_Splitter.Node_Value (Builder.Splitter, Values);

         if Integer (theTree.Values.Length) < Node_ID then
            theTree.Values.Set_Length (Count_Type (Node_ID));
         end if;
         theTree.Values.Replace_Element (Node_ID, Values);

         --  243 Add right node
         Right_Child_Cursor := Tree_Build.Add_Node
           (theTree, Builder.Splitter, Depth, Parent_Cursor, False,
            Is_Leaf_Node, Split.Feature, Impurity, Split.Threshold, Split_Row,
            End_Row, Weighted_Node_Samples);
         Node_ID := Element (Right_Child_Cursor).Node_ID;
         Put_Line ("Ada_Tree_Builder.Add_Branch right Node_ID: " &
                     Integer'Image (Node_ID));
         Node_Splitter.Node_Value (Builder.Splitter, Values);

         if Integer (theTree.Values.Length) < Node_ID then
            theTree.Values.Set_Length (Count_Type (Node_ID));
         end if;
         theTree.Values.Replace_Element (Node_ID, Values);

         --  L240
         if not Is_Leaf_Node then
            --  Add left branch
            Add_Branch (theTree, Builder, Left_Child_Cursor);
            --  Add right branch
            Add_Branch (theTree, Builder, Right_Child_Cursor);
         end if;

         --  L254
         if Depth > Max_Depth_Seen then
            Max_Depth_Seen := Depth;
         end if;
      end if;
      New_Line;

   end Add_Branch;

   --  ------------------------------------------------------------------
   --  L133 DepthFirstTreeBuilder.build
   procedure Build_Tree
     (theTree       : in out Tree.Tree_Class;
      Splitter      : in out Node_Splitter.Splitter_Class;
      Y_Encoded     : Classifier_Types.List_Of_Natural_Lists;
      Max_Depth     : Integer) is
      use Tree.Nodes_Package;
      use Node_Splitter;
      Depth            : constant Positive := 1;
      Impurity         : constant Float := Float'Last;
      Builder          : Tree_Builder;
      Top_Node_Cursor  : Cursor;
   begin
      Init_Tree_Builder (Builder, Splitter, Max_Depth => Max_Depth);

      --  L205
      --  Reset_Node resets splitter to use samples (Start_Row .. End_Row)
      Reset_Node (Splitter, 1, Positive (Y_Encoded.Length),
                  Splitter.Weighted_Samples);

      Top_Node_Cursor := Tree_Build.Add_Node
        (theTree, Splitter, Depth, theTree.Nodes.Root, True, False, 1,
         Impurity, 0.0, 1, Positive (Y_Encoded.Length),
         Splitter.Weighted_Samples);
      Add_Branch (theTree, Builder, Top_Node_Cursor);
      Put_Line ("Ada_Tree_Builder.Build_Tree tree built");
      New_Line;

   end Build_Tree;

   --  ------------------------------------------------------------------

   procedure Init_Tree_Builder
     (Builder               : in out Tree_Builder;
      Splitter              : Node_Splitter.Splitter_Class;
      Min_Samples_Split     : Positive := 2;
      Min_Samples_Leaf      : Positive := 1;
      Min_Weight_Leaf       : Float := 0.0;
      Max_Depth             : Integer := -1;
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

end Ada_Tree_Builder;
