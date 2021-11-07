--  Based on scikit-learn/sklearn/tree _tree.pyx class DepthFirstTreeBuilder

with Ada.Assertions; use Ada.Assertions;
with Ada.Containers;
--  with Ada.Text_IO; use Ada.Text_IO;

with ML_Types;
with Build_Utils;
--  with Printing;
with Tree;

package body Tree_Build is

   Epsilon   : constant Float := 10.0 ** (-10);
   Last_Node : Natural := 0;

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
   --  L720
   function Add_Node (theTree               : in out Tree.Tree_Class;
                      Depth                 : Natural;
                      Parent_Cursor         : Tree.Tree_Cursor;
                      Is_Left, Is_Leaf      : Boolean;
                      Feature_Index         : Positive;
                      Impurity, Threshold   : Float;
                      Start, Stop           : Positive;
                      Weighted_Node_Samples : Float) return Tree.Tree_Cursor is
      use Ada.Containers;
      use Tree;
      use Nodes_Package;
      New_Node    : Tree_Node (Is_Leaf);
      Node_Cursor : Tree.Tree_Cursor;
   begin
      Assert (Parent_Cursor /= No_Element,
              "Tree_Build.Add_Node, parent cursor is null.");
      Assert (Stop >= Start,
              "Tree_Build.Add_Node stop index:" & Integer'Image (Stop) &
                " is less than start index" & Integer'Image (Start));

      Last_Node := Last_Node + 1;
      New_Node.Node_ID := Last_Node;
      --  _Tree L738
      New_Node.Is_Left := Is_Left;
      New_Node.Depth := Depth;
      New_Node.Impurity := Impurity;
      New_Node.Weighted_Num_Node_Samples := Integer (Weighted_Node_Samples);

      New_Node.Samples_Start := Start;
      New_Node.Num_Node_Samples := 1 + Stop - Start;

      if not Is_Leaf then
         New_Node.Best_Fit_Feature_Index := Feature_Index;
         New_Node.Threshold := Threshold;
      end if;

      if Is_Left then
         theTree.Nodes.Prepend_Child (Parent   => Parent_Cursor,
                                      New_Item => New_Node);
         Node_Cursor := First_Child (Parent_Cursor);
      else
         theTree.Nodes.Append_Child (Parent   => Parent_Cursor,
                                     New_Item => New_Node);
         Node_Cursor := Last_Child (Parent_Cursor);
      end if;
--        Printing.Print_Node ("Tree_Build.Add_Node, New_Node", New_Node);

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
      use Ada.Containers;
      use Tree.Nodes_Package;
      Parent_Node           : constant Tree.Tree_Node :=
                                Element (Parent_Cursor);
      Is_Leaf               : Boolean;
      aSplit                : Node_Splitter.Split_Record;
      Num_Constant_Features : Natural := 0;
      Start_Row             : Positive := Parent_Node.Samples_Start;
      End_Row               : Positive :=
                                Start_Row + Parent_Node.Num_Node_Samples - 1;
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

      Is_Leaf := (Depth >= theBuilder.Max_Depth) or
        (Splitter.Num_Samples = 1 or Splitter.Num_Samples < theBuilder.Min_Samples_Split) or
        (Splitter.Num_Samples < 2 * theBuilder.Min_Samples_Leaf) or
        (Impurity <= Epsilon);

      if not Is_Leaf then
         aSplit := Node_Splitter.Split_Node (Splitter, Impurity,
                                             Num_Constant_Features);
         Is_Leaf :=
           aSplit.Improvement + Epsilon < theBuilder.Min_Impurity_Decrease;
      end if;

      if Is_Left then
         End_Row := aSplit.Split_Row - 1;
      else
         Start_Row := aSplit.Split_Row;
      end if;

      Node_Cursor := Add_Node
        (theTree, Depth, Parent_Cursor, Is_Left, Is_Leaf,
         aSplit.Feature, aSplit.Improvement, aSplit.Threshold, Start_Row,
         End_Row, Float (Parent_Node.Weighted_Num_Node_Samples));
      Node := Element (Node_Cursor);

      --  L461
      Node_Splitter.Node_Value (Splitter, Values);
      if Node.Node_ID > Integer (theTree.Values.Length) then
         theTree.Values.Set_Length (Count_Type (Node.Node_ID));
      end if;
      theTree.Values.Replace_Element (Node.Node_ID, Values);

      Res.Node_Cursor := Add_Node
        (theTree, Depth, Parent_Cursor, Is_Left, Is_Leaf,
         aSplit.Feature, Impurity, aSplit.Threshold, Start_Row, End_Row,
         Splitter.Weighted_Samples);
      Res.Node_Params := Element (Res.Node_Cursor);
      Res.Depth := Depth + 1;
      Res.Impurity := Impurity;
      Res.Is_Leaf := Is_Leaf;

      if Is_Leaf then
         Res.Position := Res.Stop;
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

   --  ------------------------------------------------------------------------
   --  The TreeBuilder recursively builds a Tree object from training samples
   --  using a Splitter object for splitting internal nodes and assigning
   --  values to leaves.
   --  Tree_Builder controls the various stopping criteria and the node splitting
   --  evaluation order, e.g. depth-first or best-first.
   procedure Build_Best_First_Tree
     (Best_Builder  : in out Tree_Builder;
      theTree       : in out Tree.Tree_Class;
      X             : ML_Types.Value_Data_Lists_2D;
      Y_Encoded     : Classifier_Types.List_Of_Natural_Lists;
      Sample_Weight : Weights.Weight_List) is
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
      Depth            : Positive := 1;
      Node_Cursor      : Tree.Tree_Cursor := theTree.Nodes.Root;
      Curs             : Frontier_Cursor;
   begin
      --  L332
      Node_Splitter.Init (Splitter, X, Y_Encoded, Sample_Weight);
      if Best_Builder.Max_Leaf_Nodes <= 0 then
         raise Tree_Build_Error with
           "Tree_Build.Build_Best_First_Tree Max_Leaf_Nodes = 0";
      end if;

      Max_Split_Nodes := Best_Builder.Max_Leaf_Nodes - 1;
      Add_Split_Node (Best_Builder, Splitter, theTree, Impurity,
                      True, True, theTree.Nodes.Root, Depth, Split_Node_Left);
      Add_To_Frontier (Split_Node_Left, Frontier);

      Curs := Frontier.First;
      while Has_Element (Curs) loop
         Heap_Record := Element (Curs);
         Current_Node := Heap_Record.Node_Params;
         Node_Cursor := Heap_Record.Node_Cursor;
         if not Current_Node.Leaf_Node then
            Max_Split_Nodes := Max_Split_Nodes - 1;
            Depth := Current_Node.Depth + 1;
            Add_Split_Node
              (Best_Builder, Splitter, theTree, Current_Node.Impurity, False,
               Current_Node.Is_Left, Node_Cursor, Depth, Split_Node_Left);
            --  tree.nodes may have changed
            Heap_Record := Element (Curs);
            Current_Node := Heap_Record.Node_Params;
            Node_Cursor := Heap_Record.Node_Cursor;
            Depth := Current_Node.Depth + 1;
            Add_Split_Node
              (Best_Builder, Splitter, theTree,
               Heap_Record.Impurity_Right, False, False, Node_Cursor, Depth,
               Split_Node_Right);
            Add_To_Frontier (Split_Node_Right, Frontier);
         end if;

         Next (Curs);
      end loop;

   end Build_Best_First_Tree;

   --  ------------------------------------------------------------------------

   procedure Build_Depth_First_Tree
     (Depth_Builder : in out Tree_Builder;
      theTree       : in out Tree.Tree_Class;
      X             : ML_Types.Value_Data_Lists_2D;
      Y_Encoded     : Classifier_Types.List_Of_Natural_Lists;
      Sample_Weight : Weights.Weight_List) is
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
      Node              : Tree.Tree_Node;
      Stack             : Stack_List;
      Data              : Stack_Record;
      Stack_Curs        : Build_Utils.Stack_Cursor;
      Node_Cursor       : Tree.Tree_Cursor := theTree.Nodes.Root;
   begin
      --  L163
      Node_Splitter.Init (Splitter, X, Y_Encoded, Sample_Weight);
      Num_Node_Samples := Natural (Splitter.Sample_Indices.Length);

      Data.Parent_Cursor := Node_Cursor;
      Data.Start := 1;
      Data.Stop := Num_Node_Samples;
      Push (Stack, 1, Num_Node_Samples, Depth, Node_Cursor, Is_Left, Impurity,
            0);

      while not Stack.Is_Empty loop
         Data := Pop (Stack);
         Start := Data.Start;
         Stop := Data.Stop;
         Depth := Data.Depth;
         Parent := Data.Parent_Cursor;
         Is_Left := Data.Is_Left;
         Constant_Features := Data.Num_Constant_Features;

         Node_Splitter.Reset_Node (Splitter, Start, Stop, Weighted_Samples);
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
            Is_Leaf := Split.Split_Row >= Stop or
              Split.Improvement + Epsilon < Depth_Builder.Min_Impurity_Decrease;
         end if;

         Node_Cursor := Add_Node
           (theTree, Depth, Parent, Is_Left, Is_Leaf,
            Split.Feature, Impurity, Split.Threshold, Start,
            Splitter.Num_Samples, Weighted_Samples);

         if not Is_Leaf then
            --  Right child
            Push (Stack, Split.Split_Row, Stop, Depth + 1, Parent,
                 False, Split.Impurity_Right, Constant_Features);

            --  Left child
            Push (Stack, Start, Split.Split_Row - 1, Depth + 1, Parent,
                 True, Split.Impurity_Left, Constant_Features);
         end if;

         if Depth > Max_Depth_Seen then
            Max_Depth_Seen := Depth;
         end if;

         Next (Stack_Curs);
      end loop;

      theTree.Max_Depth := Max_Depth_Seen;

   end Build_Depth_First_Tree;

   --  ------------------------------------------------------------------------

   procedure Change_To_Leaf_Node (aTree       : in out Tree.Tree_Class;
                                  Node_Cursor : in out Tree.Tree_Cursor) is
      use Tree.Nodes_Package;
      Old_Node  : constant Tree.Tree_Node := Element (Node_Cursor);
      Leaf_Node : Tree.Tree_Node (True);
   begin
      Leaf_Node.Node_ID := Old_Node.Node_ID;
      Leaf_Node.Impurity := Old_Node.Impurity;
      Leaf_Node.Num_Node_Samples := Old_Node.Num_Node_Samples;
      Leaf_Node. Weighted_Num_Node_Samples := Old_Node.Weighted_Num_Node_Samples;
      Leaf_Node.Samples_Start := Old_Node.Samples_Start;
      Leaf_Node.Depth := Old_Node.Depth;
      Leaf_Node.Num_Constant_Features := Old_Node.Num_Constant_Features;
--        Leaf_Node.Values := Old_Node.Values;
      aTree.Nodes.Replace_Element (Node_Cursor, Leaf_Node);

   end Change_To_Leaf_Node;

   --  ------------------------------------------------------------------------
   --  __cinit__  Only call for python declaraton constructor arguments
   procedure Init_Best_First_Tree
     (Best_Builder          : in out Tree_Builder;
      Splitter              : Node_Splitter.Splitter_Class;
      Min_Samples_Split     : Natural := 0;
      Min_Samples_Leaf      : Natural := 0;
      Min_Weight_Leaf       : Float := 0.0;
      Max_Depth             : Integer := -1;
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
      Max_Depth                           : Integer := -1;
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
