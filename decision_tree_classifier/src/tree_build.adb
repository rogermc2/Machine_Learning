--  Based on scikit-learn/sklearn/tree _tree.pxd class DepthFirstTreeBuilder
--  The TreeBuilder recursively builds a Tree object from training samples,
--  using a Splitter object for splitting internal nodes and assigning
--  values to leaves.
--  Tree_Builder controls the various stopping criteria and the node splitting
--  evaluation order, e.g. depth-first or best-first.

with Ada.Text_IO; use Ada.Text_IO;

with ML_Types;
with Priority_Heap;
with Tree;

package body Tree_Build is

   Epsilon : constant Float := 10.0 ** (-10);

   --  ------------------------------------------------------------------------

   procedure Add_Branch (Rows          : ML_Types.Rows_Vector;
                         Parent_Cursor : ML_Types.Tree_Cursor) is
      --  Parent_Cursor is a cursor to an existing node which is the head
      --  of this branch
      use ML_Types;
      use Tree_Package;
      Best_Split       : constant Best_Data := Find_Best_Split (Rows);
      Child_Cursor     : Tree_Cursor;
      True_Split_Rows  : Rows_Vector;
      False_Split_Rows : Rows_Vector;
   begin
      --           Utilities.Print_Rows ("Add_Branch Rows", Rows);
      if Best_Split.Gain = 0.0 then
         Utilities.Print_Question ("Add_Branch prediction", Best_Split.Question);
         Put_Line ("Add_Branch prediction Gini" &
                     Float'Image (Best_Split.Gini));
         New_Line;
         Add_Prediction_Node (Parent_Cursor, Rows);
      elsif Max_Leaves = 0 or else Num_Leaves < Max_Leaves then
         --              Utilities.Print_Question ("Add_Branch Best split",
         --                                        Best_Split.Question);
         Add_Decision_Node (Parent_Cursor, Best_Split);
         True_Split_Rows := Best_Split.True_Rows;
         False_Split_Rows := Best_Split.False_Rows;
         Child_Cursor := Last_Child (Parent_Cursor);
         --              Utilities.Print_Rows ("Add_Branch True_Split_Rows",
         --                                    True_Split_Rows);
         Add_Branch (True_Split_Rows, Child_Cursor);
         --              Utilities.Print_Rows ("Add_Branch False_Split_Rows",
         --                                    False_Split_Rows);
         Add_Branch (False_Split_Rows, Child_Cursor);
         --              New_Line;
      end if;
   end Add_Branch;

   --  ----------------------------------------------------------------------

   procedure Add_Decision_Node (Parent_Cursor : ML_Types.Tree_Cursor;
                                Best_Split    : Best_Data) is
      use ML_Types;
      Node  : Tree_Node_Type (Decision_Node);
   begin
      Node.Decision_Branch := True;
      Node.Question := Best_Split.Question;
      Node.True_Branch := Best_Split.True_Rows;
      Node.False_Branch := Best_Split.False_Rows;
      Node.Gini := Best_Split.Gini;
      theTree.Insert_Child (Parent   => Parent_Cursor,
                            Before   => No_Element,
                            New_Item => Node);
   end Add_Decision_Node;

   --  ----------------------------------------------------------------------

   procedure Add_Prediction_Node (Parent_Cursor : Tree.Tree_Cursor;
                                  Rows          : ML_Types.Rows_Vector) is
      use ML_Types;
      Leaf : Tree_Node_Type (Prediction_Node);
   begin
      if Max_Leaves > 0 then
         Num_Leaves := Num_Leaves + 1;
      end if;
      --           New_Line;
      Leaf.Decision_Branch := False;
      Leaf.Prediction := Rows.First_Element;
      Leaf.Rows := Rows;
      Leaf.Prediction_List := Utilities.Predictions (Leaf);
      --           Utilities.Print_Rows ("Prediction", Rows);
      --           New_Line;
      theTree.Insert_Child (Parent_Cursor, No_Element, Leaf);
   end Add_Prediction_Node;

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
   procedure Add_Node (theTree               : in out Tree.Tree_Class;
                       Parent_Cursor         : Tree.Tree_Cursor;
                       Type_Of_Feature       : Tree.Data_Type;
                       Type_Of_Node          : ML_Types.Node_Kind;
                       Is_Left, Is_Leaf      : Boolean;
                       Feature               : Tree.Features_Record;
                       Impurity, Threshold   : Float;
                       Node_Samples          : Natural;
                       Weighted_Node_Samples : Float) is
      use Tree;
      use Tree_Package;
      New_Node : Tree_Node (Is_Leaf, Type_Of_Feature);
      --        Child_Cursor : Tree_Cursor := Last_Child (Parent_Cursor);
   begin
      New_Node.Impurity := Impurity;
      New_Node.Num_Node_Samples := Node_Samples;
      New_Node.Weighted_Num_Node_Samples := Integer (Weighted_Node_Samples);

      if not Is_Leaf then
         New_Node.Feature := Feature;
         New_Node.Threshold := Threshold;
      end if;

      if Is_Left then
         theTree.Nodes.Insert_Child (Parent   => Parent_Cursor,
                                     Before   => No_Element,
                                     New_Item => New_Node);
      else
         theTree.Nodes.Append_Child (Parent   => Parent_Cursor,
                                     New_Item => New_Node);
      end if;

   end Add_Node;

   --  ------------------------------------------------------------------------

   procedure Add_Split_Node
     (theBuilder        : in out Tree_Builder;
      theTree           : in out Tree.Tree_Class;
      Parent_Cursor     : Tree.Tree_Cursor;
      Splitter          : in out Node_Splitter.Splitter_Class;
      Impurity          : in out Float;
      Is_First, Is_Left : Boolean;
      Depth             : Positive;
      Res               : in out Priority_Heap.Priority_Record) is
      Node_Samples      : Natural;
      Node_Val          : Float;
      Type_Of_Feature   : Tree.Data_Type;
      Type_Of_Node      : ML_Types.Node_Kind;
      Feature           : Tree.Features_Record (Type_Of_Feature);
      Is_Leaf           : Boolean;
      aSplit            : Node_Splitter.Split_Record;
      Constant_Features : ML_Types.Value_Data_List;
   begin
      Node_Splitter.Reset_Node (Splitter, Splitter.Weighted_Samples);
      if Is_First then
         Impurity := Splitter.Node_Impurity;
      end if;

      Is_Leaf := (Depth >= theBuilder.Max_Depth) or
        (Node_Samples < theBuilder.Min_Samples_Split) or
        (Node_Samples < 2 * theBuilder.Min_Samples_Leaf) or
        (Impurity <= Epsilon);

      if not Is_Leaf then
         aSplit := Node_Splitter.Split_Node (Splitter, Impurity,
                                             Constant_Features);
         Is_Leaf :=
           aSplit.Improvement + Epsilon < theBuilder.Min_Impurity_Decrease;
      end if;

      Add_Node (theTree, theTree.Nodes.Root, Type_Of_Feature, Type_Of_Node,
                Is_Left, Is_Leaf, Feature, Impurity, aSplit.Threshold,
                Node_Samples, Splitter.Weighted_Samples);
      Node_Splitter.Node_Value (Splitter, Node_Val);
      --          aTree.Values (1, 1, 1) := Node_Val;

      Res.Is_Leaf := Is_Leaf;
      if Is_Leaf then
         Res.Improvement := 0.0;
         Res.Impurity_Left := Impurity;
         Res.Impurity_Right := Impurity;
      else
         Res.Depth := Depth;
         Res.Improvement := aSplit.Improvement;
         Res.Impurity_Left := aSplit.Impurity_Left;
         Res.Impurity_Right := aSplit.Impurity_Right;
      end if;

   end Add_Split_Node;

   --  ------------------------------------------------------------------------

   procedure Add_To_Frontier (Rec : Priority_Heap.Priority_Record;
                              Frontier : in out Priority_Heap.Frontier_List) is
   begin
      Frontier.Append (Rec);
   end Add_To_Frontier;

   --  ------------------------------------------------------------------------

   --     procedure Build_Tree
   --       (aBuilder      : in out Tree_Builder;
   --        theTree       : in out Tree.Tree_Class;
   --        X, Y          : ML_Types.List_Of_Value_Data_Lists;
   --        Sample_Weight : Classifier_Types.Weight_List) is
   --        Max_Split_Nodes : Natural := aBuilder.Max_Leaf_Nodes -1;
   --        Splitter        : Node_Splitter.Splitter_Class;
   --        Capacity        : Positive := Max_Split_Nodes + aBuilder.Max_Leaf_Nodes;
   --     begin
   --        Node_Splitter.Init (Splitter, X, Y, Sample_Weight);
   --        Tree.Resize (theTree, Capacity);
   --
   --     end Build_Tree;

   --  ------------------------------------------------------------------------

   procedure Build_Best_First_Tree
     (Best_Builder  : in out Tree_Builder;
      theTree       : in out Tree.Tree_Class;
      X, Y          : ML_Types.List_Of_Value_Data_Lists;
      Sample_Weight : Classifier_Types.Weight_List) is
      use Priority_Heap;
      use Frontier_Package;
      use Tree.Tree_Package;
      Splitter         : Node_Splitter.Splitter_Class;
      Heap_Record      : Priority_Record;
      Split_Node_Left  : Priority_Record;
      Split_Node_Right : Priority_Record;
      Max_Split_Nodes  : Natural;
      Node             : Tree.Tree_Node;
      Impurity         : Float := 0.0;
      Frontier         : Priority_Heap.Frontier_List;
      Curs             : Frontier_Cursor;
   begin
      Init_Best_First_Tree (Best_Builder, Splitter);
      Node_Splitter.Init (Splitter, X, Y, Sample_Weight);
      Max_Split_Nodes := Best_Builder.Max_Leaf_Nodes - 1;

      Add_Split_Node (Best_Builder, theTree, theTree.Nodes.Root,
                      Splitter, Impurity, True, True, 1, Split_Node_Left);
      Add_To_Frontier (Split_Node_Left, Frontier);

      Curs := Frontier.First;
      while Has_Element (Curs) loop
         Heap_Record := Element (Curs);
         Node := Heap_Record.Node;
         if not Node.Is_Leaf then
            Max_Split_Nodes := Max_Split_Nodes - 1;
            Add_Split_Node
              (Best_Builder, theTree, Last_Child (theTree.Nodes.Root), Splitter, Node.Impurity,
               False, Node.Is_Left, Node.Depth + 1, Split_Node_Left);
            --  tree.nodes may have changed
            Heap_Record := Element (Curs);
            Node := Heap_Record.Node;
            Add_Split_Node
              (Best_Builder, theTree, Last_Child (theTree.Nodes.Root),
               Splitter, Heap_Record.Impurity_Right, False, False,
               Node.Depth + 1, Split_Node_Right);
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
      use ML_Types;
      use ML_Types.Tree_Package;
      Ada_Tree         : Tree_Type := Empty_Tree;
      Type_Of_Feature  : Tree.Data_Type;
      Feature          : Tree.Features_Record (Type_Of_Feature);
      Feature_Values   : Value_Data_List;
      Label_Values     : Value_Data_List;
      aRow             : Row_Data;
      Rows             : Rows_Vector;
      Initial_Capacity : Positive := 2047;
      Start            : Positive;
      Stop             : Positive;
      Is_First         : Boolean;
      Is_Left          : Boolean;
      Parent           : ML_Types.Tree_Node_Type;
      Impurity         : Float;
      Threshold        : Float;
      Node_Samples     : Natural;
      Weighted_Samples : Float;
      Depth            : Positive;
      Is_Leaf          : Boolean := False;
      aSplitter        : Node_Splitter.Splitter_Class;
   begin
      --  L 147
      --          if theTree.Max_Depth <= 10 then
      --              Initial_Capacity := 2 ** (theTree.Max_Depth + 1) - 1;
      --          end if;
      --
      --          Tree.Resize (theTree, Initial_Capacity);

      for index in 1 .. Positive (X.Length) loop
         Feature_Values := X.Element (index);
         Label_Values := Y.Element (index);
         for index2 in Feature_Values.First_Index ..
           Feature_Values.Last_Index loop
            aRow.Features (Class_Range (index)) :=
              Feature_Values.Element (index2).UB_String_Value;
            aRow.Label :=
              Label_Values.Element (index2).UB_String_Value;
            Rows.Append (aRow);
         end loop;
      end loop;

      Add_Node (theTree, theTree.Nodes.Root, Type_Of_Feature, Decision_Node,
                True, False, Feature, Impurity, Threshold, Node_Samples,
                Weighted_Samples);

   end Build_Depth_First_Tree;

   --  ------------------------------------------------------------------------

   procedure Init_Best_First_Tree
     (Best_Builder       : in out Tree_Builder;
      Splitter           : Node_Splitter.Splitter_Class;
      Min_Samples_Split, Min_Samples_Leaf : Natural := 0;
      Min_Weight_Leaf : Float := 0.0;
      Max_Depth, Max_Leaf_Nodes : Natural := 0;
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
     (Depth_Builder      : in out Tree_Builder;
      Splitter           : Node_Splitter.Splitter_Class;
      Min_Samples_Split, Min_Samples_Leaf : Natural := 0;
      Min_Weight_Leaf : Float := 0.0;
      Max_Depth : Natural := 0;
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
