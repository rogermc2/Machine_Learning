--  Based on scikit-learn/sklearn/tree _tree.pxd class DepthFirstTreeBuilder

with Ada.Text_IO; use Ada.Text_IO;

with ML_Types;
with Build_Utils;
--  with Utilities;
with Tree;

package body Tree_Build is

   Epsilon : constant Float := 10.0 ** (-10);

   --  ----------------------------------------------------------------------
   --  Based on scikit-learn/sklearn/tree _tree.pyx _add_node
   --  node_samples : array of int, shape [node_count]
   --  node_samples[i] holds the number of training samples reaching node i.
   --  weighted_node_samples : array of int, shape [node_count]
   --  weighted_node_samples[i] holds the weighted number of training samples
   --  reaching node i.
   --  Parent_Cursor is a cursor to an existing node which is the head
   --  of this node's branch Tree.
   --  Tree_Class.Nodes is an Ada Indefinite Multiway Tree.
   function Add_Node (theTree               : in out Tree.Tree_Class;
                      Parent_Cursor         : Tree.Tree_Cursor;
                      Is_Left, Is_Leaf      : Boolean;
                      Feature_Index         : Positive;
                      Impurity, Threshold   : Float;
                      Weighted_Node_Samples : Float) return Tree.Tree_Cursor is
      use Tree;
      use Tree_Package;
      New_Node    : Tree_Node (Is_Leaf);
      Node_Cursor : Tree.Tree_Cursor;
   begin
      --  L735
      theTree.Node_Count := theTree.Node_Count + 1;
      New_Node.Node_Index := theTree.Node_Count;
      New_Node.Impurity := Impurity;
      New_Node.Weighted_Num_Node_Samples := Integer (Weighted_Node_Samples);

      if not Is_Leaf then
         New_Node.Feature_Index := Feature_Index;
         New_Node.Threshold := Threshold;
      end if;

      if Is_Left then
         --              Put_Line ("Tree_Build.Add_Node Prepend_Child");
         theTree.Nodes.Prepend_Child (Parent   => Parent_Cursor,
                                      New_Item => New_Node);
         --              Put_Line ("Tree_Build.Add_Node Prepended");
         Node_Cursor := First_Child (Parent_Cursor);
      else
         --              Put_Line ("Tree_Build.Add_Node Append_Child");
         theTree.Nodes.Append_Child (Parent   => Parent_Cursor,
                                     New_Item => New_Node);
         --              Put_Line ("Tree_Build.Add_Node Appended");
         Node_Cursor := Last_Child (Parent_Cursor);
      end if;

      return Node_Cursor;

   end Add_Node;

   --  ------------------------------------------------------------------------

   procedure Add_Split_Node
     (theBuilder            : in out Tree_Builder;
      Splitter              : in out Node_Splitter.Splitter_Class;
      theTree               : in out Tree.Tree_Class;
      Impurity              : in out Float;
      Is_First, Is_Left     : Boolean;
      Parent_Cursor         : Tree.Tree_Cursor;
      Depth                 : Positive;
      Res                   : in out Build_Utils.Priority_Record) is

      Num_Node_Samples      : constant Natural :=
                                Splitter.End_Index - Splitter.Start_Index;
      Is_Leaf               : Boolean;
      aSplit                : Node_Splitter.Split_Record;
      Num_Constant_Features : Natural := 0;
   begin
      --  L429
      Node_Splitter.Reset_Node (Splitter, Splitter.Weighted_Samples);

      if Is_First then
         Impurity := Splitter.Node_Impurity;
      end if;

      Is_Leaf := (Depth >= theBuilder.Max_Depth) or
        (Num_Node_Samples < theBuilder.Min_Samples_Split) or
        (Num_Node_Samples < 2 * theBuilder.Min_Samples_Leaf) or
        (Impurity <= Epsilon);

      if not Is_Leaf then
         aSplit := Node_Splitter.Split_Node (Splitter, Impurity,
                                             Num_Constant_Features);
         Is_Leaf :=
           aSplit.Improvement + Epsilon < theBuilder.Min_Impurity_Decrease;
      end if;

      Res.Node_Cursor := Add_Node (theTree, Parent_Cursor, Is_Left, Is_Leaf,
                                   aSplit.Feature_Index, Impurity, aSplit.Threshold,
                                   Splitter.Weighted_Samples);
      Res.Start := Splitter.Start_Index;
      Res.Stop := Splitter.End_Index;
      Res.Depth := Depth;
      Res.Impurity := Impurity;
      Res.Is_Leaf := Is_Leaf;

      if Is_Leaf then
         Res.Position := Res.Stop;
         Res.Improvement := 0.0;
         Res.Impurity_Left := Impurity;
         Res.Impurity_Right := Impurity;
      else
         Res.Position := aSplit.Pos_I;
         Res.Improvement := aSplit.Improvement;
         Res.Impurity_Left := aSplit.Impurity_Left;
         Res.Impurity_Right := aSplit.Impurity_Right;
      end if;

   end Add_Split_Node;

   --  ------------------------------------------------------------------------

   procedure Add_To_Frontier (Rec      : Build_Utils.Priority_Record;
                              Frontier : in out Build_Utils.Frontier_List) is
   begin
      Frontier.Append (Rec);
   end Add_To_Frontier;

   --  ------------------------------------------------------------------------
   --  The TreeBuilder recursively builds a Tree object from training samples
   --  using a Splitter object for splitting internal nodes and assigning
   --  values to leaves.
   --  Tree_Builder controls the various stopping criteria and the node splitting
   --  evaluation order, e.g. depth-first or best-first.
   procedure Build_Best_First_Tree
     (Best_Builder  : in out Tree_Builder;
      theTree       : in out Tree.Tree_Class;
      X, Y          : ML_Types.List_Of_Value_Data_Lists;
      Sample_Weight : Classifier_Types.Weight_List) is
      use Build_Utils;
      use Frontier_Package;
      Splitter         : Node_Splitter.Splitter_Class;
      Heap_Record      : Priority_Record;
      Split_Node_Left  : Priority_Record;
      Split_Node_Right : Priority_Record;
      Max_Split_Nodes  : Natural;
      Impurity         : Float := 0.0;
      Frontier         : Build_Utils.Frontier_List;
      Current_Node     : Tree.Tree_Node;
      Node_Cursor      : Tree.Tree_Cursor := theTree.Nodes.Root;
      Curs             : Frontier_Cursor;
   begin
      --  L332
      Node_Splitter.Init (Splitter, X, Y, Sample_Weight);
      if Best_Builder.Max_Leaf_Nodes <= 0 then
         raise Tree_Build_Error with
           "Tree_Build.Build_Best_First_Tree Max_Leaf_Nodes = 0";
      end if;

      Put_Line ("Tree_Build.Build_Best_First_Tree Max_Leaf_Nodes: " &
                  Integer'Image (Best_Builder.Max_Leaf_Nodes));
      Max_Split_Nodes := Best_Builder.Max_Leaf_Nodes - 1;
      Add_Split_Node (Best_Builder, Splitter, theTree, Impurity,
                      True, True, theTree.Nodes.Root, 1, Split_Node_Left);
      Add_To_Frontier (Split_Node_Left, Frontier);

      Curs := Frontier.First;
      while Has_Element (Curs) loop
         Heap_Record := Element (Curs);
         Current_Node := Heap_Record.Node_Params;
         Node_Cursor := Heap_Record.Node_Cursor;
         if not Current_Node.Is_Leaf then
            Put_Line ("Tree_Build.Build_Best_First_Tree Max_Leaf_Nodes: " &
                        Integer'Image (Max_Split_Nodes));
            Max_Split_Nodes := Max_Split_Nodes - 1;
            Add_Split_Node
              (Best_Builder, Splitter, theTree, Current_Node.Impurity,
               False, Current_Node.Is_Left, Node_Cursor,
               Current_Node.Depth + 1, Split_Node_Left);
            --  tree.nodes may have changed
            Heap_Record := Element (Curs);
            Current_Node := Heap_Record.Node_Params;
            Node_Cursor := Heap_Record.Node_Cursor;
            Add_Split_Node
              (Best_Builder, Splitter, theTree,
               Heap_Record.Impurity_Right, False, False, Node_Cursor,
               Current_Node.Depth + 1, Split_Node_Right);
            Add_To_Frontier (Split_Node_Right, Frontier);
         end if;

         Next (Curs);
      end loop;

   end Build_Best_First_Tree;

   --  ------------------------------------------------------------------------

   procedure Build_Depth_First_Tree
     (Depth_Builder : in out Tree_Builder;
      theTree       : in out Tree.Tree_Class;
      X, Y          : ML_Types.List_Of_Value_Data_Lists;
      Sample_Weight : Classifier_Types.Weight_List) is
      use Build_Utils;
      use Build_Utils.Stack_Package;
      First             : Boolean := True;
      Is_Left           : Boolean := True;
      Parent            : Tree.Tree_Cursor;
      Impurity          : Float := Float'Last;
      Num_Node_Samples  : Natural := 0;
      Weighted_Samples  : Float := 0.0;
      Constant_Features : Natural := 0;
      Start             : Positive := 1;
      Stop              : Positive := 1;
      Depth             : Natural := 0;
      Max_Depth_Seen    : Natural := 0;
      Is_Leaf           : Boolean := False;
      Splitter          : Node_Splitter.Splitter_Class;
      Split             : Node_Splitter.Split_Record;
      Node              : Stack_Record;
      Stack             : Stack_List;
      Stack_Curs        : Build_Utils.Stack_Cursor;
      Node_Cursor       : Tree.Tree_Cursor := theTree.Nodes.Root;
   begin
      --  L163
      Node_Splitter.Init (Splitter, X, Y, Sample_Weight);
      Num_Node_Samples := Splitter.Num_Samples;
      Put_Line ("Tree_Build.Build_Depth_First_Tree Num_Node_Samples: " &
                  Integer'Image (Integer (Num_Node_Samples)));

      Node.Parent := Node_Cursor;
      Node.Start := 1;
      Node.Stop := Num_Node_Samples;
      Stack.Append (Node);
      Stack_Curs := Stack.First;
      Put_Line ("Tree_Build.Build_Depth_First_Tree Stack size: " &
                  Integer'Image (Integer (Stack.Length)));

      while Has_Element (Stack_Curs) loop
         Node := Element (Stack_Curs);
         Start := Node.Start;
         Stop := Node.Stop;
         Depth := Node.Depth;
         Parent := Node.Parent;
         Is_Left := Node.Is_Left;
         Constant_Features := Node.Num_Constant_Features;

         Node_Splitter.Reset_Node (Splitter, Weighted_Samples);
         if First then
            Impurity := Splitter.Node_Impurity;
            First := False;
         else
            Impurity := Node.Impurity;
         end if;

         Is_Leaf := Depth >= Depth_Builder.Max_Depth or
           Num_Node_Samples < Depth_Builder.Min_Samples_Split or
           Num_Node_Samples < 2 * Depth_Builder.Min_Samples_Leaf or
           Weighted_Samples < 2.0 * Depth_Builder.Min_Weight_Leaf or
           Impurity <= Epsilon;

         if not Is_Leaf then
            Split := Node_Splitter.Split_Node (Splitter, Impurity,
                                               Constant_Features);
            Is_Leaf := Split.Pos_I >= Stop or
              Split.Improvement + Epsilon < Depth_Builder.Min_Impurity_Decrease;
         end if;

         Put_Line ("Tree_Build.Build_Depth_First_Tree Add_Node");
         Node_Cursor := Add_Node (theTree, Parent, Is_Left, Is_Leaf,
                                  Split.Feature_Index, Impurity,
                                  Split.Threshold, Weighted_Samples);
         Put_Line ("Tree_Build.Build_Depth_First_Tree Node added, Is_Left, Is_Leaf: "
                   & Boolean'Image (Is_Left) & ", " & Boolean'Image (Is_Leaf));

         if not Is_Leaf then
            --  Right child
            Node.Parent := Parent;
            Node.Node_Cursor := Node_Cursor;
            Node.Start := Split.Pos_I;
            Node.Stop := Stop;
            Node.Depth := Depth + 1;
            Node.Is_Left := False;
            Node.Impurity := Split.Impurity_Right;
            Node.Num_Constant_Features := Constant_Features;
            Stack.Append (Node);

            --  Left child
            Node.Start := Start;
            Node.Stop := Split.Pos_I;
            Node.Depth := Depth + 1;
            Node.Is_Left := True;
            Node.Impurity := Split.Impurity_Left;
            Stack.Append (Node);
         end if;

         if Depth > Max_Depth_Seen then
            Max_Depth_Seen := Depth;
         end if;
         Put_Line ("Tree_Build.Build_Depth_First_Tree loop end Stack size: " &
                     Integer'Image (Integer (Stack.Length)));

         Next (Stack_Curs);
      end loop;

      theTree.Max_Depth := Max_Depth_Seen;

   end Build_Depth_First_Tree;

   --  ------------------------------------------------------------------------
   --  __cinit__  Only call for python declaraton constructor arguments
   procedure Init_Best_First_Tree
     (Best_Builder          : in out Tree_Builder;
      Splitter              : Node_Splitter.Splitter_Class;
      Min_Samples_Split     : Natural := 0;
      Min_Samples_Leaf      : Natural := 0;
      Min_Weight_Leaf       : Float := 0.0;
      Max_Depth             : Natural := 0;
      Max_Leaf_Nodes        : Integer := -1;
      Min_Impurity_Decrease : Float := 0.0) is
   begin
      Best_Builder.Splitter := Splitter;
      Best_Builder.Min_Samples_Split := Min_Samples_Split;
      Best_Builder.Min_Samples_Leaf := Min_Samples_Leaf;
      Best_Builder.Min_Weight_Leaf := Min_Weight_Leaf;
      Best_Builder.Max_Depth := Max_Depth;
      Best_Builder.Max_Leaf_Nodes := Max_Leaf_Nodes;
      Best_Builder.Min_Impurity_Decrease := Min_Impurity_Decrease;

   end Init_Best_First_Tree;

   --  ------------------------------------------------------------------------

   procedure Init_Depth_First_Tree
     (Depth_Builder                       : in out Tree_Builder;
      Splitter                            : Node_Splitter.Splitter_Class;
      Min_Samples_Split, Min_Samples_Leaf : Natural := 0;
      Min_Weight_Leaf                     : Float := 0.0;
      Max_Depth                           : Natural := 0;
      Min_Impurity_Decrease               : Float := 0.0) is
   begin
      Depth_Builder.Splitter := Splitter;
      Depth_Builder.Min_Samples_Split := Min_Samples_Split;
      Depth_Builder.Min_Samples_Leaf := Min_Samples_Leaf;
      Depth_Builder.Min_Weight_Leaf := Min_Weight_Leaf;
      Depth_Builder.Max_Depth := Max_Depth;
      Depth_Builder.Min_Impurity_Decrease := Min_Impurity_Decrease;

   end Init_Depth_First_Tree;

   --  ------------------------------------------------------------------------

end Tree_Build;
