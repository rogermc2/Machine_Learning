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

   --  ------------------------------------------------------------------------

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
      Num_Samples           : constant Positive :=
                                End_Row - Start_Row + 1;
      Weighted_Node_Samples : Float := Splitter.Weighted_Samples;
      Is_Leaf               : Boolean;
      aSplit                : Node_Splitter.Split_Record;
      Num_Constant_Features : Natural := 0;
      Node_Cursor           : Tree.Tree_Cursor;
      Node                  : Tree.Tree_Node;
      Values                : Weights.Weight_Lists_2D;
   begin
      --  L429
      Node_Splitter.Reset_Node (Splitter, Start_Row, End_Row,
                                Weighted_Node_Samples);
      if Is_First then
         Impurity := Splitter.Node_Impurity;
      end if;

      --  L435
      Is_Leaf := Depth > theBuilder.Max_Depth or
        Num_Samples = 1 or Num_Samples < theBuilder.Min_Samples_Split or
        Num_Samples < 2 * theBuilder.Min_Samples_Leaf or
        Weighted_Node_Samples < 2.0 * theBuilder.Min_Weight_Leaf or
        Impurity <= Epsilon;

      if not Is_Leaf then
         aSplit := Node_Splitter.Split_Node (Splitter, Impurity,
                                             Num_Constant_Features);
         Is_Leaf :=
           aSplit.Split_Row > End_Row or
           aSplit.Improvement + Epsilon < theBuilder.Min_Impurity_Decrease;
      end if;

      --  L449
      Node_Cursor := Tree_Build.Add_Node
        (theTree, Parent_Cursor, Branch, Is_Leaf, aSplit.Feature,
         aSplit.Threshold, Impurity, Splitter.Num_Samples,
         Weighted_Node_Samples);
      Node := Element (Node_Cursor);

      --  L459
      Node_Splitter.Node_Value (Splitter, Values);
      if Node.Node_ID > Integer (theTree.Values.Length) then
         theTree.Values.Set_Length (Count_Type (Node.Node_ID));
      end if;
      theTree.Values.Replace_Element (Node.Node_ID, Values);

      Res.Node_Cursor := Node_Cursor;
--        Res.Node_Params := Element (Node_Cursor);
      Res.Start := Start_Row;
      Res.Stop_Row := End_Row;
      Res.Depth := Depth;
      Res.Impurity := Impurity;
      Res.Is_Leaf := Is_Leaf;

      if Is_Leaf then
         --  L477
         Res.Position := End_Row;
         Res.Improvement := 0.0;
         Res.Impurity_Left := Impurity;
         Res.Impurity_Right := Impurity;
      else
         --  L468
         Res.Position := aSplit.Split_Row;
         Res.Improvement := aSplit.Improvement;
         Res.Impurity_Left := aSplit.Impurity_Left;
         Res.Impurity_Right := aSplit.Impurity_Right;
      end if;

   end Add_Split_Node;

   --  ------------------------------------------------------------------------

   --  The best node to expand is given by the node at the frontier that has
   --  the highest impurity improvement.
   procedure Build_Tree
     (Builder       : in out Tree_Build.Tree_Builder;
      theTree       : in out Tree.Tree_Class) is
      use Tree.Nodes_Package;
      use Build_Utils;
      use Frontier_Package;
      Routine_Name          : constant String := "Best_First_Builder.Build_Tree ";
      Splitter              : Node_Splitter.Splitter_Class := Builder.Splitter;
      Start_Row             : constant Positive := 1;
      Stop_Row              : constant Positive := Builder.Splitter.Num_Samples;
      Depth                 : constant Natural := 1;
      Impurity_Left         : constant Float := Float'Last;
      Impurity_Right        : constant Float := Float'Last;
      Improvement           : constant Float := -Float'Last;
      Weighted_Node_Samples : Float := 0.0;
      Heap_Record           : Priority_Record;
      Split_Node_Left       : Priority_Record;
      Split_Node_Right      : Priority_Record;
      Max_Split_Nodes       : Natural;
      Frontier              : Build_Utils.Frontier_List;
      Stack                 : Frontier_List;
      Node_Cursor           : Tree.Tree_Cursor := theTree.Nodes.Root;
      Node                  : Tree.Tree_Node;
      Curs                  : Frontier_Cursor := Frontier.First;
      Is_Leaf               : Boolean := False;
   begin
      Put_Line (Routine_Name);
      Assert (Builder.Max_Leaf_Nodes > 0, Routine_Name & "Max_Leaf_Nodes = 0");
      Max_Split_Nodes := Builder.Max_Leaf_Nodes - 1;

      --  L335
      Push (Stack, False, Start_Row, Start_Row, Start_Row + 1, Depth, theTree.Nodes.Root,
            Impurity_Left, Impurity_Right, Improvement);
--        Put_Line (Routine_Name & "Frontier Has_Element " & Boolean'Image ( Has_Element (Curs)));
      --  L345
      while Has_Element (Curs) loop
         --  Reset_Node resets splitter to use samples (Start_Row .. End_Row)
         Node_Splitter.Reset_Node (Builder.Splitter, Start_Row, Stop_Row,
                                   Weighted_Node_Samples);
         Heap_Record := Pop (Stack);
         Node_Cursor := Heap_Record.Node_Cursor;
         Node := Element (Node_Cursor);
         --  L349
            Put_Line ("Max_Split_Nodes: " & Integer'Image (Max_Split_Nodes));
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
            Push (Frontier, Split_Node_Left);
            Push (Frontier, Split_Node_Right);
         end if;

         if Heap_Record.Depth > Max_Depth_Seen then
            Max_Depth_Seen := Heap_Record.Depth;
         end if;
         theTree.Max_Depth := Max_Depth_Seen;

         Next (Curs);
      end loop;

      Put_Line ("Best first tree built.");
      Put_Line ("Num nodes: " &
                  Integer'Image (Integer (theTree.Nodes.Node_Count)));

   end Build_Tree;

   --  ------------------------------------------------------------------------

end Best_First_Builder;
