--  Based on scikit-learn/sklearn/tree _tree.pyx L119
--  class DepthFirstTreeBuilder

with Ada.Assertions; use Ada.Assertions;
with Ada.Containers;
with Ada.Text_IO; use Ada.Text_IO;

with Build_Utils;
with Node_Splitter;
--  with Printing;
with Tree;
with Weights;

package body Best_First_Builder is

   Epsilon : constant Float := 10.0 ** (-10);

   Max_Depth_Seen : Positive := 1;

   --  ------------------------------------------------------------------
   --  Based on scikit-learn/sklearn/tree _tree.pyx BestFirstTreeBuilder.build
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
                                "Best_First_Builder.Add_Branch ";
      --  L199
      Data                  : constant Stack_Record := Pop (theStack);
      Start_Row             : constant Positive := Data.Start;
      Stop_Row              : constant Positive := Data.Stop;
      Num_Node_Samples      : constant Positive := Stop_Row - Start_Row + 1;
      Impurity              : Float;
      Num_Constant_Features : Natural := Data.Num_Constant_Features;
      Is_Leaf_Node          : Boolean := False;
      Weighted_Node_Samples : Float := 0.0;
      Values                : Weights.Weight_Lists_2D;
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
        (Builder.Max_Depth > 0 and then Data.Depth > Builder.Max_Depth) or
        Builder.Splitter.Num_Samples = 1 or
        Num_Node_Samples < Builder.Min_Samples_Split or
        Num_Node_Samples < 2 * Builder.Min_Samples_Leaf or
        Weighted_Node_Samples < 2.0 * Builder.Min_Weight_Leaf;

      if Builder.Max_Depth > 0 and then Data.Depth > Builder.Max_Depth then
         null;
         --           Put_Line (Routine_Name &
         --                       " L207 Leaf_Node Data.Depth " &
         --                       Integer'Image (Data.Depth) & " >= Builder.Max_Depth " &
         --                       Integer'Image (Builder.Max_Depth));
      elsif Builder.Splitter.Num_Samples = 1 then
         Put_Line (Routine_Name &
                     " L207 Leaf_Node Builder.Splitter.Num_Samples = 1");
         --        elsif Num_Node_Samples < Builder.Min_Samples_Split then
         --           Put_Line (Routine_Name & " L207 Leaf_Node Num_Node_Samples <" &
         --                       " Builder.Min_Samples_Split: "  &
         --                       Integer'Image (Builder.Min_Samples_Split));
         --        elsif Num_Node_Samples < 2 * Builder.Min_Samples_Leaf then
         --           Put_Line (Routine_Name & " L207 Leaf_Node Num_Node_Samples < " &
         --                       "2 * Builder.Min_Samples_Leaf");
      elsif Weighted_Node_Samples < 2.0 * Builder.Min_Weight_Leaf then
         Put_Line
           (Routine_Name & " L207 Leaf_Node Weighted_Node_Samples < " &
              "2.0 * Builder.Min_Weight_Leaf");
         --        elsif abs (Impurity) <= Epsilon then
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
      --  Values dimensions: num outputs x num classes
      Node_Splitter.Node_Value (Builder.Splitter, Values);

      if Node_ID > Integer (theTree.Values.Length) then
         theTree.Values.Set_Length (Count_Type (Node_ID));
      end if;
      theTree.Values.Replace_Element (Node_ID, Values);

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

   --     procedure Add_To_Frontier (Rec      : Build_Utils.Priority_Record;
   --                                Frontier : in out Build_Utils.Frontier_List) is
   --     begin
   --        Frontier.Append (Rec);
   --     end Add_To_Frontier;

   --  ------------------------------------------------------------------------
   --  L300 BestFirstTreeBuilder.build
   --     procedure Build_Tree
   --       (theTree               : in out Tree.Tree_Class;
   --        Builder               : in out Tree_Build.Tree_Builder;
   --        Y_Encoded             : Classifier_Types.Natural_Lists_2D) is
   --        use Build_Utils;
   --  --        use Frontier_Package;
   --        use Tree.Nodes_Package;
   --        Routine_Name      : constant String := "Best_First_Builder.Build_Tree ";
   --        Depth             : constant Natural := 1;
   --        Start_Row         : constant Positive := 1;
   --        Stop_Row          : constant Positive := Positive (Y_Encoded.Length);
   --        First             : Boolean := True;
   --        Impurity          : constant Float := Float'Last;
   --  --        Frontier          : Build_Utils.Frontier_List;
   --  --        Curs              : Frontier_Cursor;
   --        Constant_Features : constant Natural := 0;
   --        Stack             : Stack_List;
   --        Split             : Node_Splitter.Split_Record;
   --     begin
   --        --  L344
   --        Push (Stack, Start_Row, Stop_Row, Depth, theTree.Nodes.Root,
   --              Tree.Left_Node, Impurity, Constant_Features);
   --
   --        --  L190
   --        while not Stack.Is_Empty loop
   --           Add_Branch (theTree, Builder, Stack, Split, First);
   --        end loop;
   --
   --        Put_Line (Routine_Name & "tree built.");
   --        New_Line;
   --
   --     end Build_Tree;

   --  ------------------------------------------------------------------

   procedure Build_Tree
     (Builder       : in out Tree_Build.Tree_Builder;
      theTree       : in out Tree.Tree_Class;
      X             : ML_Types.Value_Data_Lists_2D;
      Y_Encoded     : Classifier_Types.Natural_Lists_2D;
      Sample_Weight : Weights.Weight_List) is
      use Build_Utils;
      use Frontier_Package;
      Routine_Name     : constant String := "Best_First_Builder.Build_Tree ";
      Splitter         : Node_Splitter.Splitter_Class;
      Heap_Record      : Priority_Record;
      Split_Node_Left  : Priority_Record;
      Split_Node_Right : Priority_Record;
      Max_Split_Nodes  : Natural;
      Impurity         : Float := 0.0;
      Frontier         : Build_Utils.Frontier_List;
      --        Current_Node     : Tree.Tree_Node;
      Node_Cursor      : Tree.Tree_Cursor := theTree.Nodes.Root;
      Curs             : Frontier_Cursor;
      Is_Leaf          : Boolean := False;
   begin
      --  L324
      Node_Splitter.Initialize_Splitter (Splitter, X, Y_Encoded, Sample_Weight);
      Assert (Builder.Max_Leaf_Nodes > 0, Routine_Name & "Max_Leaf_Nodes = 0");

      Max_Split_Nodes := Best_Builder.Max_Leaf_Nodes - 1;
      --  L344 add root to frontier
      Add_Split_Node
        (Best_Builder, Splitter, theTree, 1, Splitter.Num_Samples, Impurity,
         True, Tree.Left_Node, theTree.Nodes.Root, 0, Split_Node_Left);
      Add_To_Frontier (Split_Node_Left, Frontier);

      --  L354
      Curs := Frontier.First;
      while Has_Element (Curs) loop
         Heap_Record := Element (Curs);
         Node_Cursor := Heap_Record.Node_Cursor;
         Is_Leaf := Heap_Record.Is_Leaf or Max_Split_Nodes < 0;
         if not Is_Leaf then
            --  L371
            Max_Split_Nodes := Max_Split_Nodes - 1;
            Add_Split_Node
              (Best_Builder, Splitter, theTree, Heap_Record.Start,
               Heap_Record.Position, Heap_Record.Impurity, False,
               Tree.Left_Node, Node_Cursor, Heap_Record.Depth + 1, Split_Node_Left);
            --  L383 tree.nodes may have changed
            Heap_Record := Element (Curs);
            Node_Cursor := Heap_Record.Node_Cursor;
            --  Compute right split node
            Add_Split_Node
              (Best_Builder, Splitter, theTree, Heap_Record.Position,
               Heap_Record.Stop_Row, Heap_Record.Impurity_Right, False,
               Tree.Right_Node, Node_Cursor, Heap_Record.Depth + 1,
               Split_Node_Right);
            Add_To_Frontier (Split_Node_Right, Frontier);
         end if;

         Next (Curs);
      end loop;

   end Build_Tree;

   --  ------------------------------------------------------------------------

end Best_First_Builder;
