--  Based on scikit-learn/sklearn/tree _tree.pyx class DepthFirstTreeBuilder

with Ada.Assertions; use Ada.Assertions;
--  with Ada.Text_IO; use Ada.Text_IO;

--  with Printing;
with Tree;

package body Tree_Build is

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
    --  L716
    function Add_Node (theTree               : in out Tree.Tree_Class;
                       Parent_Cursor         : Tree.Tree_Cursor;
                       Branch                : Tree.Node_Type;
                       Is_Leaf               : Boolean;
                       Feature_Index         : Positive;
                       Threshold, Impurity   : Float;
                       Num_Samples           : Positive;
                       Weighted_Node_Samples : Float) return Tree.Tree_Cursor is
        use Tree;
        use Nodes_Package;
        Routine_Name : constant String := "Tree_Build.Add_Node ";
        New_Node    : Tree_Node (Is_Leaf);
        Node_Cursor : Tree.Tree_Cursor;
    begin
        Assert (Parent_Cursor /= No_Element,
                Routine_Name & "parent cursor is null.");

        Last_Node := Last_Node + 1;
        New_Node.Node_ID := Last_Node;
        --  _Tree L738
        New_Node.Impurity := Impurity;
        New_Node.Num_Node_Samples := Num_Samples;
        New_Node.Weighted_Num_Node_Samples := Integer (Weighted_Node_Samples);

        if not Is_Leaf then
            New_Node.Best_Fit_Feature_Index := Feature_Index;
            New_Node.Threshold := Threshold;
        end if;

        case Branch is
            when Left_Node =>
                theTree.Nodes.Prepend_Child
                  (Parent   => Parent_Cursor, New_Item => New_Node);
                Node_Cursor := First_Child (Parent_Cursor);
            when Top_Node | Right_Node =>
                theTree.Nodes.Append_Child
                  (Parent => Parent_Cursor, New_Item => New_Node);
                Node_Cursor := Last_Child (Parent_Cursor);
        end case;

        return Node_Cursor;

    end Add_Node;

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
        Leaf_Node. Weighted_Num_Node_Samples :=
          Old_Node.Weighted_Num_Node_Samples;
        aTree.Nodes.Replace_Element (Node_Cursor, Leaf_Node);

    end Change_To_Leaf_Node;

    --  ------------------------------------------------------------------------

   procedure Init_Builder
     (Builder               : in out Tree_Build.Tree_Builder;
      Max_Leaf_Nodes        : Integer;
      Splitter              : Node_Splitter.Splitter_Class;
      Min_Samples_Split     : Natural := 0;
      Min_Samples_Leaf      : Natural := 0;
      Min_Weight_Leaf       : Float := 0.0;
      Max_Depth             : Integer := -1;
      Min_Impurity_Decrease : Float := 0.0) is
   begin
      if Max_Leaf_Nodes < 0 then
         declare
            Build : Tree_Builder (Depth_First_Tree);
         begin
            Builder := Build;
         end;
      else
         declare
            Build : Tree_Builder (Best_First_Tree);
         begin
            Build.Max_Leaf_Nodes := Max_Leaf_Nodes;
            Builder := Build;
         end;
      end if;

      Builder.Splitter := Splitter;
      Builder.Min_Samples_Split := Min_Samples_Split;
      Builder.Min_Samples_Leaf := Min_Samples_Leaf;
      Builder.Min_Weight_Leaf := Min_Weight_Leaf;
      Builder.Max_Depth := Max_Depth;
      Builder.Min_Impurity_Decrease := Min_Impurity_Decrease;
      Tree_Build.Reset_Last_Node;

   end Init_Builder;

   --  -------------------------------------------------------------------------

    procedure Reset_Last_Node is
    begin
        Last_Node := 0;
    end Reset_Last_Node;

    --  ------------------------------------------------------------------------

end Tree_Build;
