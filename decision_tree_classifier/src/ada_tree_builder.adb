--  Based on scikit-learn/sklearn/tree _tree.pyx class DepthFirstTreeBuilder

with Ada.Assertions; use Ada.Assertions;
with Ada.Text_IO; use Ada.Text_IO;

--  with Classifier_Utilities;
with Node_Splitter;
with Tree;
with Tree_Build;

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
      use Node_Splitter;
      use Tree;
      use Nodes_Package;
      Splitter               : Node_Splitter.Splitter_Class :=
                                 Builder.Splitter;
      Split                  : Split_Record;
      --  Parent_Node corresponds to popped stack_record?
      --  L199
      Parent_Node            : Tree.Tree_Node := Element (Parent_Cursor);
      Start_Row              : constant Positive := Parent_Node.Samples_Start;
      End_Row                : constant Positive :=
                                 Start_Row + Parent_Node.Num_Node_Samples - 1;
      Depth                  : constant Positive := Parent_Node.Depth + 1;
      Num_Constant_Features  : Natural := Parent_Node.Num_Constant_Features;
      Is_Leaf                : Boolean := False;
      Impurity               : Float := Float'Last;
      Weighted_Node_Samples  : Float := 0.0;
      Left_Child_Cursor      : Tree.Tree_Cursor;
      Right_Child_Cursor     : Tree.Tree_Cursor;
      Left_Child             : Tree.Tree_Node;
      Right_Child            : Tree.Tree_Node;
      Split_Row              : Positive := End_Row;
   begin
      if not Parent_Node.Is_Leaf then
         --           Put_Line ("Ada_Tree_Builder.Add_Branch entry, Start_Row, End_Row, Num_Node_Samples: "
         --                     & Integer'Image (Start_Row) & ", " & Integer'Image (End_Row)  &
         --                       ", " & Integer'Image (Parent_Node.Num_Node_Samples));
         --  L208
         --  Reset_Node resets splitter to use samples (Start_Row .. End_Row)
         Reset_Node (Splitter, Start_Row, End_Row, Weighted_Node_Samples);
         --              Put_Line ("Ada_Tree_Builder.Add_Branch Start_Row, End_Row, Num_Node_Samples reset: "
         --                        & Integer'Image (Start_Row) & ", " & Integer'Image (End_Row)  &
         --                          ", " & Integer'Image (Parent_Node.Num_Node_Samples));

         if First then
            Impurity := Node_Impurity (Splitter);
            First := False;
         end if;

         --  L210
         Is_Leaf := Depth >= Builder.Max_Depth or Splitter.Num_Samples = 1 or
           Parent_Node.Num_Node_Samples < Builder.Min_Samples_Split or
           Parent_Node.Num_Node_Samples < 2 * Builder.Min_Samples_Leaf or
           Weighted_Node_Samples < 2.0 * Builder.Min_Weight_Leaf or
           --  if Impurity == 0.0 with tolerance for rounding errors
         abs (Impurity) <= Epsilon;

         --  L222
         if Is_Leaf then
            Tree_Build.Change_To_Leaf_Node (theTree, Parent_Cursor);
            Parent_Node := Element (Parent_Cursor);
            --                  Classifier_Utilities.Print_Node
            --                    ("Ada_Tree_Builder.Add_Branch, L222 else changed to Leaf Node", Parent_Node);
         else  --  L222 not leaf
            Split := Split_Node (Splitter, Impurity, Num_Constant_Features);
            Split_Row := Split.Split_Row;
            --                  Put_Line ("Ada_Tree_Builder.Add_Branch L222 not leaf Start_Row, End_Row "
            --                            & Integer'Image (Start_Row) & ", " & Integer'Image (End_Row));
            Assert (Split_Row > Start_Row and Split_Row <= End_Row,
                    "Ada_Tree_Builder.Add_Branch L222, Split_Row index " &
                      Integer'Image (Split_Row) &
                      " is not in the row index range " &
                      Integer'Image (Start_Row + 1) & " .. " &
                      Integer'Image (End_Row));

            --                  Classifier_Utilities.Print_Split_Record
            --                    ("Ada_Tree_Builder.Add_Branch, Split", Split);

            Is_Leaf := Split_Row = Parent_Node.Samples_Start or
              Split_Row >= End_Row or
              Split.Improvement + Epsilon <= Builder.Min_Impurity_Decrease;

            --  L229  _tree.add_node just generates a new initialized node
            --        right and left children are added to the tree (stack) at
            --        L245 and L251 respectively
            --                  Put_Line ("Ada_Tree_Builder.Add_Branch L229 adding node, Start_Row, Split_Row, Num_Node_Samples:"
            --                            & Integer'Image (Start_Row) & ", " & Integer'Image (Split_Row) &
            --                              ", " & Integer'Image (Parent_Node.Num_Node_Samples));
            Put_Line ("Ada_Tree_Builder.Add_Branch L229, adding left node");
            Left_Child_Cursor := Tree_Build.Add_Node
              (theTree, Splitter, Depth, Parent_Cursor, True, Is_Leaf,
               Split.Feature, Impurity, Split.Threshold, Start_Row,
               Split_Row - 1, Weighted_Node_Samples);
            --  L241 Node.Values already added by Tree_Build.Add_Node

            Left_Child := Element (Left_Child_Cursor);

            --  L254
            if Depth > Max_Depth_Seen then
               Max_Depth_Seen := Depth;
            end if;
            --  Add right node
            --                  Put_Line ("Ada_Tree_Builder.Add_Branch L254, Split_Row, End_Row:"
            --                            & Integer'Image (Split_Row) & Integer'Image (End_Row));
            Put_Line ("Ada_Tree_Builder.Add_Branch L254, adding right node");
            Right_Child_Cursor := Tree_Build.Add_Node
              (theTree, Splitter, Depth, Parent_Cursor, False, False,
               Split.Feature, Impurity, Split.Threshold, Split_Row, End_Row,
               Weighted_Node_Samples);
            Right_Child := Element (Right_Child_Cursor);

            if Depth > Max_Depth_Seen then
               Max_Depth_Seen := Depth;
            end if;

            if not Left_Child.Is_Leaf then
               --  Add left branch
               Add_Branch (theTree, Builder, Left_Child_Cursor);
            end if;

            if not Right_Child.Is_Leaf then
               --  Add right branch
               Add_Branch (theTree, Builder, Right_Child_Cursor);
            end if;
         end if;
      end if;

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
      Builder          : Tree_Builder;
      Top_Node_Cursor  : Cursor;
   begin
      Init_Tree_Builder (Builder, Splitter, Max_Depth => Max_Depth);

      --  L208
      --  Reset_Node resets splitter to use samples (Start_Row .. End_Row)
      Reset_Node (Splitter, 1, Positive (Y_Encoded.Length),
                  Splitter.Weighted_Samples);

      Top_Node_Cursor := Tree_Build.Add_Node
        (theTree, Splitter, Depth, theTree.Nodes.Root, True, False, 1,
         Float'Last, 0.0, 1, Positive (Y_Encoded.Length),
         Splitter.Weighted_Samples);
      Add_Branch (theTree, Builder, Top_Node_Cursor);

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
