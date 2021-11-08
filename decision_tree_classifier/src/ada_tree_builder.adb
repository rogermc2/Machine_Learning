--  Based on scikit-learn/sklearn/tree _tree.pyx L119 class DepthFirstTreeBuilder

--  with Ada.Assertions; use Ada.Assertions;
with Ada.Containers;
with Ada.Text_IO; use Ada.Text_IO;

with Build_Utils;
with Node_Splitter;
--  with Printing;
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
     (theTree  : in out Tree.Tree_Class;
      Builder  : in out Tree_Builder;
      theStack : in out Build_Utils.Stack_List;
      Split    : in out Node_Splitter.Split_Record) is
      use Ada.Containers;
      use Build_Utils;
      use Node_Splitter;
      use Tree;
      use Nodes_Package;
      --  L199
      Data                  : Stack_Record := Pop (theStack);
      Start_Row             : constant Positive := Data.Start;
      End_Row               : constant Positive := Data.Stop;
      Num_Node_Samples      : constant Positive := End_Row - Start_Row + 1;
      Num_Constant_Features : Natural := Data.Num_Constant_Features;
      Is_Leaf_Node          : Boolean := False;
      Impurity              : Float := Float'Last;
      Weighted_Node_Samples : Float := 0.0;
      Values                : Weights.Weight_Lists_2D;
      Child_Cursor          : Tree.Tree_Cursor;
      Node_ID               : Positive;
   begin
      --  L209
      --  Reset_Node resets splitter to use samples (Start_Row .. End_Row)
      Reset_Node (Builder.Splitter, Start_Row, End_Row, Weighted_Node_Samples);
      --  L216
      Impurity := Data.Impurity;

      --  L207
      Is_Leaf_Node := Data.Depth >= Builder.Max_Depth or
        Builder.Splitter.Num_Samples = 1 or
        Num_Node_Samples < Builder.Min_Samples_Split or
        Num_Node_Samples < 2 * Builder.Min_Samples_Leaf or
        Weighted_Node_Samples < 2.0 * Builder.Min_Weight_Leaf or
        --  if Impurity == 0.0 with tolerance for rounding errors
      abs (Impurity) <= Epsilon;

      --        Put_Line ("Ada_Tree_Builder.Add_Branch L222 Is_Leaf_Node: " &
      --                    Boolean'Image (Is_Leaf_Node));
      --  L220
      if Is_Leaf_Node then
         Tree_Build.Change_To_Leaf_Node (theTree, Data.Parent_Cursor);
      else
         Split := Split_Node (Builder.Splitter, Impurity,
                              Num_Constant_Features);
--           Printing.Print_Split_Record ("Ada_Tree_Builder.Add_Branch L220 Split",
--                                        Split);
         --  L233
         Is_Leaf_Node := Split.Split_Row >= End_Row or
           Split.Improvement + Epsilon < Builder.Min_Impurity_Decrease;
         --           Put_Line ("Ada_Tree_Builder.Add_Branch L228 Split.Improvement: " &
         --                       Float'Image (Split.Improvement));
      end if;

      --  tree.add_node just generates a new initialized node
      --  right and left children are added to the tree (stack) at
      --  L245 and L251 respectively
      --  L228
      --        Put_Line ("Ada_Tree_Builder.Add_Branch L228 Builder Start, Pos, End: " &
      --                    Integer'Image (Builder.Splitter.Start_Row) & ", " &
      --                    Integer'Image (Split.Split_Row) & ", " &
      --                    Integer'Image (Builder.Splitter.End_Row));
      if First then
         Child_Cursor := Data.Parent_Cursor;
         First := False;
      else
         Child_Cursor := Tree_Build.Add_Node
           (theTree, Data.Depth, Data.Parent_Cursor, Data.Is_Left, Is_Leaf_Node,
            Split.Feature, Impurity, Split.Threshold, Start_Row,
            Split.Split_Row, Weighted_Node_Samples);
      end if;
      Node_ID := Element (Child_Cursor).Node_ID;
      --  238
      Node_Splitter.Node_Value (Builder.Splitter, Values);
      if Node_ID > Integer (theTree.Values.Length) then
         theTree.Values.Set_Length (Count_Type (Node_ID));
      end if;
      theTree.Values.Replace_Element (Node_ID, Values);

      --  L240
      if not Is_Leaf_Node then
         --  Add right branch
         Push (theStack, Split.Split_Row, End_Row, Data.Depth + 1,
               Child_Cursor, False, Split.Impurity_Right,
               Num_Constant_Features);
         --  Add left branch
         Push (theStack, Start_Row, Split.Split_Row - 1, Data.Depth + 1,
               Child_Cursor, True, Split.Impurity_Left,
               Num_Constant_Features);
      end if;
      --  L254
      if Data.Depth + 1 > Max_Depth_Seen then
         Max_Depth_Seen := Data.Depth + 1;
      end if;

   end Add_Branch;

   --  ------------------------------------------------------------------
   --  L133 DepthFirstTreeBuilder.build
   procedure Build_Tree
     (theTree       : in out Tree.Tree_Class;
      Splitter      : in out Node_Splitter.Splitter_Class;
      Y_Encoded     : Classifier_Types.List_Of_Natural_Lists;
      Max_Depth     : Integer) is
      use Build_Utils;
      use Tree.Nodes_Package;
      use Node_Splitter;
      Depth            : constant Natural := 1;
      Builder          : Tree_Builder;
      Start_Row        : constant Positive := 1;
      End_Row          : constant Positive := Positive (Y_Encoded.Length);
      Impurity         : Float;
      Weighted_Samples : Float := 0.0;
      Stack            : Stack_List;
      Split            : Node_Splitter.Split_Record;
      Top_Node_Cursor  : Cursor;
   begin
      Init_Tree_Builder (Builder, Splitter, Max_Depth => Max_Depth);
      Reset_Node (Builder.Splitter, Start_Row, End_Row, Weighted_Samples);
      Impurity := Gini_Node_Impurity (Builder.Splitter);

      Top_Node_Cursor := Tree_Build.Add_Node
        (theTree, Depth, theTree.Nodes.Root, True, False, 1, Impurity, 0.0,
         Start_Row, End_Row, Splitter.Weighted_Samples);
      Push (Stack, Start_Row, End_Row, Depth, Top_Node_Cursor, True,
            Impurity, 0);
      New_Line;
      while not Stack.Is_Empty loop
         Add_Branch (theTree, Builder, Stack, Split);
      end loop;
      Put_Line ("Ada_Tree_Builder.Build_Tree tree built.");
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
