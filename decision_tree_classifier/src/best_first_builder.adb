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

   procedure Add_Split_Node
     (theBuilder         : in out Tree_Build.Tree_Builder;
      Splitter           : in out Node_Splitter.Splitter_Class;
      theTree            : in out Tree.Tree_Class;
      Start_Row, End_Row : Positive;
      Impurity           : in out Float;
      Is_First           : Boolean;
      Branch             : Tree.Node_Type;
      Parent_Cursor      : Tree.Tree_Cursor;
      Depth              : Natural;
      Res                : in out Build_Utils.Priority_Record) is
      use Ada.Containers;
      use Tree.Nodes_Package;
--        Routine_Name          : constant String :=
--                                  "Best_First_Builder.Add_Split_Node ";
      Parent_Node           : constant Tree.Tree_Node :=
                                Element (Parent_Cursor);
      Num_Samples           : constant Positive :=
                                End_Row - Start_Row + 1;
      Is_Leaf               : Boolean;
      aSplit                : Node_Splitter.Split_Record;
      Num_Constant_Features : Natural := 0;
      Node_Cursor           : Tree.Tree_Cursor;
      Node                  : Tree.Tree_Node;
      Values                : Weights.Weight_Lists_2D;
   begin
      --  L429
      Node_Splitter.Reset_Node (Splitter, Start_Row, End_Row,
                                Splitter.Weighted_Samples);
      if Is_First then
         Impurity := Splitter.Node_Impurity;
      end if;

      --  L440
      Is_Leaf := (Depth > theBuilder.Max_Depth) or
        (Num_Samples = 1 or Num_Samples < theBuilder.Min_Samples_Split) or
        (Num_Samples < 2 * theBuilder.Min_Samples_Leaf) or
        (Impurity <= Epsilon);

      if not Is_Leaf then
         aSplit := Node_Splitter.Split_Node (Splitter, Impurity,
                                             Num_Constant_Features);
         Is_Leaf :=
           aSplit.Split_Row > End_Row or
           aSplit.Improvement + Epsilon < theBuilder.Min_Impurity_Decrease;
      end if;

      Node_Cursor := Tree_Build.Add_Node
        (theTree, Parent_Cursor, Branch, Is_Leaf, aSplit.Feature,
         aSplit.Threshold, Impurity, Splitter.Num_Samples,
         Float (Parent_Node.Weighted_Num_Node_Samples));
      Node := Element (Node_Cursor);

      --  L461
      Node_Splitter.Node_Value (Splitter, Values);
      if Node.Node_ID > Integer (theTree.Values.Length) then
         theTree.Values.Set_Length (Count_Type (Node.Node_ID));
      end if;
      theTree.Values.Replace_Element (Node.Node_ID, Values);

      Res.Node_Cursor := Tree_Build.Add_Node
        (theTree, Parent_Cursor, Branch, Is_Leaf, aSplit.Feature,
         aSplit.Threshold, Impurity, Splitter.Num_Samples,
         Splitter.Weighted_Samples);
      Res.Node_Params := Element (Res.Node_Cursor);
      Res.Depth := Depth + 1;
      Res.Impurity := Impurity;
      Res.Is_Leaf := Is_Leaf;

      if Is_Leaf then
         Res.Position := Res.Stop_Row;
         Res.Improvement := 0.0;
         Res.Impurity_Left := Impurity;
         Res.Impurity_Right := Impurity;
      else
         Res.Position := End_Row;
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

   --  ------------------------------------------------------------------
   --  The best node to expand is given by the node at the frontier that has
   --  the highest impurity improvement.
   procedure Build_Tree
     (Builder       : in out Tree_Build.Tree_Builder;
      theTree       : in out Tree.Tree_Class) is
      use Tree.Nodes_Package;
      use Build_Utils;
      use Frontier_Package;
      Routine_Name     : constant String := "Best_First_Builder.Build_Tree ";
      Splitter         : Node_Splitter.Splitter_Class := Builder.Splitter;
      Heap_Record      : Priority_Record;
      Split_Node_Left  : Priority_Record;
      Split_Node_Right : Priority_Record;
      Max_Split_Nodes  : Natural;
      Impurity         : Float := 0.0;
      Frontier         : Build_Utils.Frontier_List;
      Node_Cursor      : Tree.Tree_Cursor := theTree.Nodes.Root;
      Node             : Tree.Tree_Node;
      Curs             : Frontier_Cursor := Frontier.First;
      Is_Leaf          : Boolean := False;
   begin
      Assert (Builder.Max_Leaf_Nodes > 0, Routine_Name & "Max_Leaf_Nodes = 0");
      Max_Split_Nodes := Builder.Max_Leaf_Nodes - 1;

      --  L344 add root to frontier
      Append_Child (theTree.Nodes, theTree.Nodes.Root, Node);
      Node_Cursor := First_Child (theTree.Nodes.Root);
      Add_Split_Node
        (Builder, Splitter, theTree, 1, Splitter.Num_Samples, Impurity,
         True, Tree.Left_Node, Node_Cursor, 0, Split_Node_Left);
      Add_To_Frontier (Split_Node_Left, Frontier);

      --  L345
      while Has_Element (Curs) loop
         Heap_Record := Element (Curs);
         Node_Cursor := Heap_Record.Node_Cursor;
         Node := Element (Node_Cursor);
         --  L349
         Is_Leaf := Heap_Record.Is_Leaf or Max_Split_Nodes < 0;
         if Is_Leaf then
            Tree_Build.Change_To_Leaf_Node (theTree, Node_Cursor);
         else --  Node is expandable
            --  L362
            Max_Split_Nodes := Max_Split_Nodes - 1;
            Add_Split_Node
              (Builder, Splitter, theTree, Heap_Record.Start,
               Heap_Record.Position, Heap_Record.Impurity, False,
               Tree.Left_Node, Node_Cursor, Heap_Record.Depth + 1, Split_Node_Left);
            --  L374 tree.nodes may have changed
            Heap_Record := Element (Curs);
            Node_Cursor := Heap_Record.Node_Cursor;
            --  L378 Compute right split node
            Add_Split_Node
              (Builder, Splitter, theTree, Heap_Record.Position,
               Heap_Record.Stop_Row, Heap_Record.Impurity_Right, False,
               Tree.Right_Node, Node_Cursor, Heap_Record.Depth + 1,
               Split_Node_Right);
            Add_To_Frontier (Split_Node_Left, Frontier);
            Add_To_Frontier (Split_Node_Right, Frontier);
         end if;

         if Heap_Record.Depth > Max_Depth_Seen then
            Max_Depth_Seen := Heap_Record.Depth;
         end if;
         theTree.Max_Depth := Max_Depth_Seen;

         Next (Curs);
      end loop;
      Put_Line ("Best first tree built.");

   end Build_Tree;

   --  ------------------------------------------------------------------------

end Best_First_Builder;
