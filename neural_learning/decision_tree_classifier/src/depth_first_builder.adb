--  Based on scikit-learn/sklearn/tree _tree.pyx L119
--  class DepthFirstTreeBuilder

with Ada.Containers;
with Ada.Text_IO; use Ada.Text_IO;

with Build_Utils;
--  with Printing;
with Weights;

package body Depth_First_Builder is

   Epsilon : constant Float := 10.0 ** (-10);

   Max_Depth_Seen : Positive := 1;

   --  ------------------------------------------------------------------
   --  Based on scikit-learn/sklearn/tree _tree.pyx
   --  DepthFirstTreeBuilder.build
   procedure Add_Branch
     (theTree  : in out Tree.Tree_Class;
      Builder  : in out Tree_Build.Tree_Builder;
      theStack : in out Build_Utils.Stack_List;
      Split    : in out Node_Splitter.Split_Record;
      First    : in out Boolean) is
      use Ada.Containers;
      use Build_Utils;
      use Node_Splitter;
      use Tree;
      use Nodes_Package;
      Routine_Name          : constant String :=
                                "Depth_First_Builder.Add_Branch ";
      --  L199
      Data                  : constant Stack_Record := Pop (theStack);
      Start_Row             : constant Positive := Data.Start;
      Stop_Row              : constant Positive := Data.Stop;
      Num_Node_Samples      : constant Positive := Stop_Row - Start_Row + 1;
      Impurity              : Float := Data.Impurity;
      Num_Constant_Features : Natural := Data.Num_Constant_Features;
      Is_Leaf_Node          : Boolean := False;
      Weighted_Node_Samples : Float := 0.0;
      Node_Weights          : Weights.Weight_List;
      Child_Cursor          : Tree.Tree_Cursor;
      Node_ID               : Positive := 1;
   begin
      --  L202
      --  Reset_Node resets splitter to use samples (Start_Row .. End_Row)
      Reset_Node (Builder.Splitter, Start_Row, Stop_Row,
                  Weighted_Node_Samples);
      --        Put_Line (Routine_Name & " Start_Row, Stop_Row" &
      --                       Integer'Image (Start_Row) & ", " &
      --                       Integer'Image (Stop_Row));

      --  L204
      Is_Leaf_Node :=
        (Builder.Max_Depth > 0 and Data.Depth > Builder.Max_Depth) or
        Builder.Splitter.Num_Samples = 1 or
        Num_Node_Samples < Builder.Min_Samples_Split or
        Num_Node_Samples < 2 * Builder.Min_Samples_Leaf or
        Weighted_Node_Samples < 2.0 * Builder.Min_Weight_Leaf;

      if Builder.Max_Depth > 0 and then
        Data.Depth > Builder.Max_Depth then
         null;
         Put_Line (Routine_Name &
                     " L207 Leaf_Node Data.Depth " &
                     Integer'Image (Data.Depth) & ">= Builder.Max_Depth " &
                     Integer'Image (Builder.Max_Depth));
      elsif Builder.Splitter.Num_Samples = 1 then
         Put_Line (Routine_Name &
                     " L207 Leaf_Node Builder.Splitter.Num_Samples = 1");
      elsif Num_Node_Samples < Builder.Min_Samples_Split then
         Put_Line (Routine_Name & " L207 Leaf_Node Num_Node_Samples <" &
                     " Builder.Min_Samples_Split: "  &
                     Integer'Image (Builder.Min_Samples_Split));
      elsif Num_Node_Samples < 2 * Builder.Min_Samples_Leaf then
         Put_Line (Routine_Name & " L207 Leaf_Node Num_Node_Samples < " &
                     "2 * Builder.Min_Samples_Leaf");
      elsif Weighted_Node_Samples < 2.0 * Builder.Min_Weight_Leaf then
         Put_Line
           (Routine_Name & " L207 Leaf_Node Weighted_Node_Samples < " &
              "2.0 * Builder.Min_Weight_Leaf");
--        elsif not First and then abs (Impurity) <= Epsilon then
--           Put_Line (Routine_Name &
--                       " L207 Leaf_Node abs (Impurity" & Float'Image (Impurity) &
--                       ") <= Epsilon");
      end if;

      --  L209
      if First then
         Impurity := Gini_Node_Impurity (Builder.Splitter);
         First := False;
      else
         Impurity := Data.Impurity;
      end if;

      Is_Leaf_Node := Is_Leaf_Node or
      abs (Impurity) <= Epsilon;  --  0.0 with tolerance for rounding errors

      --  L216
      if not Is_Leaf_Node then
         Split := Split_Node (Builder.Splitter, Impurity,
                              Num_Constant_Features);
         --  L221
         Is_Leaf_Node := Split.Split_Row > Stop_Row or
           Split.Improvement + Epsilon < Builder.Min_Impurity_Decrease;
      end if;

      --  tree.add_node adds one node to the tree
      --  right and left children are added to the stack at
      --  L238 and L245 respectively
      --  L225 Add a node to theTree
      Child_Cursor := Tree_Build.Add_Node
        (theTree, Data.Parent_Cursor, Data.Branch, Is_Leaf_Node,
         Split.Feature, Split.Threshold, Impurity, Num_Node_Samples,
         Weighted_Node_Samples);
      Node_ID := Element (Child_Cursor).Node_ID;

      --  238
      Node_Splitter.Node_Value (Builder.Splitter, Node_Weights);

      if Node_ID > Integer (theTree.Values.Length) then
         theTree.Values.Set_Length (Count_Type (Node_ID));
      end if;
      theTree.Values.Replace_Element (Node_ID, Node_Weights);

      --  L240
      if not Is_Leaf_Node then
         --  Add right branch
         Push (theStack, Split.Split_Row, Stop_Row, Data.Depth + 1,
               Child_Cursor, Tree.Right_Node, Split.Impurity_Right,
               Num_Constant_Features);
         --  Add left branch
         Push (theStack, Start_Row, Split.Split_Row - 1, Data.Depth + 1,
               Child_Cursor, Tree.Left_Node, Split.Impurity_Left,
               Num_Constant_Features);
      end if;

      --  L254
      if Data.Depth + 1 > Max_Depth_Seen then
         Max_Depth_Seen := Data.Depth + 1;
      end if;
      theTree.Max_Depth := Max_Depth_Seen;

   end Add_Branch;

   --  ------------------------------------------------------------------
   --  L129 DepthFirstTreeBuilder.build
   procedure Build_Tree
     (theTree   : in out Tree.Tree_Class;
      Builder   : in out Tree_Build.Tree_Builder;
      Y_Encoded : Natural_List) is
      use Build_Utils;
      use Tree.Nodes_Package;
      Routine_Name      : constant String := "Depth_First_Builder.Build_Tree ";
      Depth             : constant Natural := 1;
      Start_Row         : constant Positive := 1;
      Stop_Row          : constant Positive := Positive (Y_Encoded.Length);
      First             : Boolean := True;
      Impurity          : constant Float := Float'Last;
      Constant_Features : constant Natural := 0;
      Stack             : Stack_List;
      Split             : Node_Splitter.Split_Record;
   begin
      --  L184
      Push (Stack, Start_Row, Stop_Row, Depth, theTree.Nodes.Root,
            Tree.Left_Node, Impurity, Constant_Features);

      --  L190
      while not Stack.Is_Empty loop
         Add_Branch (theTree, Builder, Stack, Split, First);
      end loop;

      Put_Line (Routine_Name & "tree built.");
      New_Line;

   end Build_Tree;

   --  ------------------------------------------------------------------

end Depth_First_Builder;