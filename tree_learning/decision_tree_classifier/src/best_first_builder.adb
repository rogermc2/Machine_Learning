--  Based on scikit-learn/sklearn/tree _tree.pyx L119
--  class DepthFirstTreeBuilder

with Ada.Assertions; use Ada.Assertions;
with Ada.Containers;
with Ada.Text_IO; use Ada.Text_IO;

with Build_Utils;
with Node_Splitter;
--  with Printing;
with Weights;

package body Best_First_Builder is

   Epsilon : constant Float := 10.0 ** (-10);

   Max_Depth_Seen : Positive := 1;

   procedure Add_Split_Node
     (Builder            : in out Tree_Build.Tree_Builder;
      Splitter           : in out Node_Splitter.Splitter_Class;
      theTree            : in out Tree.Tree_Class;
      Start_Row, End_Row : Positive;
      Impurity           : in out Float;
      First              : Boolean;
      Branch             : Tree.Node_Type;
      Parent_Cursor      : Tree.Tree_Cursor;
      Depth              : Natural;
      Res                : in out Build_Utils.Priority_Record);
   pragma Inline (Add_Split_Node);

   --  ------------------------------------------------------------------------

   procedure Add_Branch
     (theTree         : in out Tree.Tree_Class;
      Builder         : in out Tree_Build.Tree_Builder;
      Splitter        : in out Node_Splitter.Splitter_Class;
      Frontier        : in out Build_Utils.Frontier_List;
      Max_Split_Nodes : in out Integer;
      First           : in out Boolean) is
      use Build_Utils;
      use Tree;
      use Nodes_Package;
      --        Routine_Name          : constant String :=
      --                                  "Best_First_Builder.Add_Branch ";
      --  L346
      Data                  : Priority_Record := Pop (Frontier);
      Start_Row             : constant Positive := Data.Start;
      Stop_Row              : constant Positive := Data.Stop_Row;
      Split_Node_Left       : Priority_Record;
      Split_Node_Right      : Priority_Record;
      Is_Leaf               : Boolean := False;
      Weighted_Node_Samples : Float := 0.0;
      Node_Cursor           : Tree.Tree_Cursor;
      Node                  : Tree.Tree_Node;
   begin
      --  Reset_Node resets splitter to use samples (Start_Row .. End_Row)
      Node_Splitter.Reset_Node (Builder.Splitter, Start_Row, Stop_Row,
                                Weighted_Node_Samples);
      --  L348
      Node_Cursor := Data.Node_Cursor;
      if not First then
         Node := Element (Node_Cursor);
      end if;

      --  L349
      Is_Leaf := Data.Is_Leaf or Max_Split_Nodes <= 0;
      if Is_Leaf then
         Tree_Build.Change_To_Leaf_Node (theTree, Node_Cursor);
      else --  Node is expandable
         --  L362 Decrement number of split nodes available
         Max_Split_Nodes := Max_Split_Nodes - 1;
         --  Compute left split node
         Add_Split_Node
           (Builder, Splitter, theTree, Data.Start,
            Data.Position - 1, Data.Impurity, First,
            Tree.Left_Node, Node_Cursor, Data.Depth + 1, Split_Node_Left);
         Push (Frontier, Split_Node_Left);

         if First then
            First := False;
         else
            --  L378 Compute right split node
            Add_Split_Node
              (Builder, Splitter, theTree, Data.Position, Data.Stop_Row,
               Data.Impurity, First, Tree.Right_Node, Node_Cursor,
               Data.Depth + 1, Split_Node_Right);
            Push (Frontier, Split_Node_Right);
         end if;

         --  L392
      end if;

      if Data.Depth > Max_Depth_Seen then
         Max_Depth_Seen := Data.Depth;
      end if;
      theTree.Max_Depth := Max_Depth_Seen;

   end Add_Branch;

   --  ------------------------------------------------------------------------
   --  L438
   procedure Add_Split_Node
     (Builder            : in out Tree_Build.Tree_Builder;
      Splitter           : in out Node_Splitter.Splitter_Class;
      theTree            : in out Tree.Tree_Class;
      Start_Row, End_Row : Positive;
      Impurity           : in out Float;
      First              : Boolean;
      Branch             : Tree.Node_Type;
      Parent_Cursor      : Tree.Tree_Cursor;
      Depth              : Natural;
      Res                : in out Build_Utils.Priority_Record) is
      use Ada.Containers;
      use Tree.Nodes_Package;
      Routine_Name          : constant String :=
                                "Best_First_Builder.Add_Split_Node ";
      Weighted_Node_Samples : Float := Splitter.Weighted_Samples;
      Num_Samples           : Positive;
      Is_Leaf               : Boolean := False;
      aSplit                : Node_Splitter.Split_Record;
      Num_Constant_Features : Natural := 0;
      Node_Cursor           : Tree.Tree_Cursor;
      Node                  : Tree.Tree_Node;
      Values                : Weights.Weight_Lists_2D;
   begin
      Assert (End_Row >= Start_Row,
              Routine_Name & "End_Row " & Integer'Image (End_Row) &
                " < " & "Start_Row " & Integer'Image (Start_Row));
      Num_Samples := End_Row - Start_Row + 1;
      --  L429
      Node_Splitter.Reset_Node (Splitter, Start_Row, End_Row,
                                Weighted_Node_Samples);
      --  L457
      if First then
         Impurity := Node_Splitter.Gini_Node_Impurity (Builder.Splitter);
      end if;

      --  L461
      Is_Leaf := (Builder.Max_Depth > 0 and then Depth > Builder.Max_Depth) or
        Num_Samples = 1 or Num_Samples < Builder.Min_Samples_Split or
        Num_Samples < 2 * Builder.Min_Samples_Leaf or
        Weighted_Node_Samples < 2.0 * Builder.Min_Weight_Leaf or
      abs (Impurity) <= Epsilon;

      --        Put_Line (Routine_Name & "L435 Is_Leaf: " & Boolean'Image (Is_Leaf));
      --        Put_Line (Routine_Name & "L435 Depth, Max_Depth: " &
      --                    Integer'Image (Depth) & ", " &
      --                    Integer'Image (Builder.Max_Depth));
      --        Put_Line (Routine_Name & "L435 Num_Samples, Min_Samples_Split: " &
      --                    Integer'Image (Num_Samples) & ", " &
      --                    Integer'Image (Builder.Min_Samples_Split));

      --  L468
      if not Is_Leaf then
         aSplit :=
           Node_Splitter.Split_Node (Splitter, Impurity, Num_Constant_Features);
         Is_Leaf :=
           aSplit.Split_Row > End_Row or
           aSplit.Improvement + Epsilon < Builder.Min_Impurity_Decrease;
      end if;

      --  L475
      Node_Cursor := Tree_Build.Add_Node
        (theTree, Parent_Cursor, Branch, Is_Leaf, aSplit.Feature,
         aSplit.Threshold, Impurity, Num_Samples, Weighted_Node_Samples);
      Node := Element (Node_Cursor);

      --  L486
      Node_Splitter.Node_Value (Splitter, Values);
      if Node.Node_ID > Integer (theTree.Values.Length) then
         theTree.Values.Set_Length (Count_Type (Node.Node_ID));
      end if;
      theTree.Values.Replace_Element (Node.Node_ID, Values);

      --  L487
      Res.Node_Cursor := Node_Cursor;
      Res.Start := Start_Row;
      Res.Stop_Row := End_Row;
      Res.Depth := Depth;
      Res.Impurity := Impurity;
      Res.Is_Leaf := Is_Leaf;

      if Is_Leaf then
         --  L502
         Res.Position := End_Row;
         Res.Improvement := 0.0;
      else
         --  L495
         Res.Position := aSplit.Split_Row;
         Res.Improvement := aSplit.Improvement;
      end if;

   end Add_Split_Node;

   --  ------------------------------------------------------------------------

   --  The best node to expand is given by the node at the frontier that has
   --  the highest impurity improvement.
   procedure Build_Tree
     (Builder : in out Tree_Build.Tree_Builder;
      theTree : in out Tree.Tree_Class) is
      use Tree.Nodes_Package;
      use Build_Utils;
      use Frontier_Package;
      Routine_Name     : constant String :=
                           "Best_First_Builder.Build_Tree ";
      Splitter         : Node_Splitter.Splitter_Class := Builder.Splitter;
      Start_Row        : constant Positive := 1;
      Stop_Row         : constant Positive := Splitter.Num_Samples;
      Depth            : constant Natural := 1;
      Impurity         : constant Float := Float'Last;
      Improvement      : constant Float := -Float'Last;
      Is_Leaf          : constant Boolean := False;
      Weighted_Samples : Float := 0.0;
      Max_Split_Nodes  : Integer;
      Frontier         : Frontier_List;
      First            : Boolean := True;
   begin
      --  L315 Splitter is initialzed in Base_Decision_Tree.Base_Fit
      Assert (Builder.Max_Leaf_Nodes > 1,
              Routine_Name & "Max_Leaf_Nodes = 0");
      --  L323 Number of split nodes available
      Max_Split_Nodes := Builder.Max_Leaf_Nodes;
      Node_Splitter.Reset_Node (Builder.Splitter, Start_Row, Stop_Row,
                                Weighted_Samples);
      --  L339
      Push (Frontier, Is_Leaf, Start_Row, Stop_Row, Stop_Row + 1, Depth,
            theTree.Nodes.Root, Impurity, Improvement);

      --  L345
      while not Frontier.Is_Empty loop
         Add_Branch (theTree, Builder, Splitter, Frontier, Max_Split_Nodes,
                     First);
      end loop;

      Put_Line ("Best first tree built with" &
                  Integer'Image (Integer (theTree.Nodes.Node_Count) - 1) &
                  " nodes.");
      New_Line;
   end Build_Tree;

   --  ------------------------------------------------------------------------

end Best_First_Builder;