--  Based on scikit-learn/sklearn/tree _tree.pyx class DepthFirstTreeBuilder

with Ada.Text_IO; use Ada.Text_IO;

with Classifier_Utilities;
with Node_Splitter;
with Tree;
with Tree_Build;

package body Ada_Tree_Builder is

    Epsilon : constant Float := 10.0 ** (-10);

    First          : Boolean := True;
    Depth          : Natural := 0;
    Max_Depth_Seen : Natural := 0;

    procedure Init_Tree_Builder
      (Builder               : in out Tree_Builder;
       Splitter              : Node_Splitter.Splitter_Class;
       Min_Samples_Split     : Natural := 0;
       Min_Samples_Leaf      : Natural := 0;
       Min_Weight_Leaf       : Float := 0.0;
       Max_Depth             : Integer := -1;
       Min_Impurity_Decrease : Float := 0.0);

    --  ------------------------------------------------------------------
    --  Based on scikit-learn/sklearn/tree _tree.pyx DepthFirstTreeBuilder.build
    procedure Add_Branch
      (theTree               : in out Tree.Tree_Class;
       Builder               : in out Tree_Builder;
       Parent_Cursor         : in out Tree.Tree_Cursor) is
    --  Parent_Cursor is a cursor to an existing node which is the head
    --  of this branch
        use Node_Splitter;
        use Tree;
        use Nodes_Package;
        Splitter              : Node_Splitter.Splitter_Class :=
                                  Builder.Splitter;
        Split                 : Split_Record;
        --  Parent_Node corresponds to popped stack_record?
        --  L199
        Parent_Node           : constant Tree.Tree_Node := Element (Parent_Cursor);
        Start                 : constant Positive := Parent_Node.Samples_Start;
        Stop                  : Positive;
        Num_Node_Samples      : Positive;
        Num_Constant_Features : Natural := Parent_Node.Num_Constant_Features;
        Is_Leaf               : Boolean := False;
        Impurity              : Float := Float'Last;
        Weighted_Node_Samples : Float := 0.0;
        --          Depth                 : Natural := Parent_Node.Depth
        Left_Child_Cursor     : Tree.Tree_Cursor;
        Right_Child_Cursor    : Tree.Tree_Cursor;
        Position              : Positive;
    begin
        Put_Line ("Ada_Tree_Builder.Add_Branch Start, parent Num_Node_Samples: "
                  & Integer'Image (Start) & ", " &
                  Integer'Image (Parent_Node.Num_Node_Samples));
        Stop := Parent_Node.Num_Node_Samples + 1 - Start;
        Num_Node_Samples := Stop - Start + 1;  --  L207
        --  L208
        --  Reset_Node resets splitter to use samples (Start .. Stop)
        Reset_Node (Splitter, Start, Stop, theTree.Classes,
                    Weighted_Node_Samples);
        Put_Line ("Ada_Tree_Builder.Add_Branch Start, Stop, Num_Node_Samples: "
                  & Integer'Image (Start) & ", " & Integer'Image (Stop)  &
                    ", " & Integer'Image (Parent_Node.Num_Node_Samples));

        if First then
            Impurity := Node_Impurity (Splitter);
            First := False;
        end if;

        --  L210
        Is_Leaf := Parent_Node.Is_Leaf or else
          (Depth >= Builder.Max_Depth or
             Num_Node_Samples < Builder.Min_Samples_Split or
               Num_Node_Samples < 2 * Builder.Min_Samples_Leaf or
                 Weighted_Node_Samples < 2.0 * Builder.Min_Weight_Leaf or
                 --  if Impurity == 0.0 with tolerance for rounding errors
           abs (Impurity) <= Epsilon);

        --  L222
        if not Is_Leaf then
            Split := Split_Node (Splitter, Impurity, Num_Constant_Features);
            Classifier_Utilities.Print_Split_Record
              ("Ada_Tree_Builder.Add_Branch, Split", Split);
            Position := Split.Pos_I;
            if Position <= Parent_Node.Samples_Start then
                raise Ada_Tree_Build_Error with
                  "Ada_Tree_Builder.Add_Branch, invalid Split.Pos_I" &
                  Integer'Image (Split.Pos_I) &
                  " should be greater than Samples_Start" &
                  Integer'Image (Parent_Node.Samples_Start);
            end if;
            Is_Leaf := Position = Parent_Node.Samples_Start or
              Position >= Stop or
              Split.Improvement + Epsilon <= Builder.Min_Impurity_Decrease;
            Put_Line ("Ada_Tree_Builder.Add_Branch start, Position: " &
                        Integer'Image (Parent_Node.Samples_Start) & ", " &
                        Integer'Image (Position));
        end if;

        --  L229  _tree.add_node just generates a new initialized node
        --        right and left children are added to the tree (stack) at
        --        L245 and L251 respectively

        Left_Child_Cursor := Tree_Build.Add_Node
          (theTree, Splitter, Depth, Parent_Cursor, True, Is_Leaf,
           Split.Feature, Impurity, Split.Threshold, Parent_Node.Samples_Start,
           Position - 1, Weighted_Node_Samples);

        --  L241 Node.Values already added by Tree_Build.Add_Node

        --  L254
        if Depth > Max_Depth_Seen then
            Max_Depth_Seen := Depth;
        end if;

        if not Is_Leaf then
            Right_Child_Cursor := Tree_Build.Add_Node
              (theTree, Splitter, Depth, Parent_Cursor, False, Is_Leaf,
               Split.Feature, Impurity, Split.Threshold, Position,
               Parent_Node.Num_Node_Samples, Weighted_Node_Samples);

            if Depth > Max_Depth_Seen then
                Max_Depth_Seen := Depth;
            end if;

            if not Element (Left_Child_Cursor).Is_Leaf then
                --  Add left branch
                Add_Branch (theTree, Builder, Left_Child_Cursor);
            end if;

            if not Element (Right_Child_Cursor).Is_Leaf then
                --  Add right branch
                Add_Branch (theTree, Builder, Right_Child_Cursor);
            end if;
        end if;

    end Add_Branch;

    --  ------------------------------------------------------------------

    procedure Build_Tree
      (theTree       : in out Tree.Tree_Class;
       X             : ML_Types.List_Of_Value_Data_Lists;
       Y_Encoded     : Classifier_Types.List_Of_Natural_Lists;
       Max_Depth     : Integer;
       Sample_Weight : Weights.Weight_List) is
        use Tree.Nodes_Package;
        use Node_Splitter;
        Builder          : Tree_Builder;
        Splitter         : Splitter_Class;
        Top_Node_Cursor  : Cursor;
    begin
        --  L163
        Node_Splitter.Init (Splitter, X, Y_Encoded, Sample_Weight);
        Init_Tree_Builder (Builder, Splitter, Max_Depth => Max_Depth);

        --  L208
        --  Reset_Node resets splitter to use samples (Start .. Stop)
        Reset_Node (Splitter, 1, Splitter.Num_Samples, theTree.Classes,
                    Splitter.Weighted_Samples);

        Top_Node_Cursor := Tree_Build.Add_Node
          (theTree, Splitter, Depth, theTree.Nodes.Root, True, False, 1,
           Float'Last, 0.0, 1, Splitter.Num_Samples, Splitter.Weighted_Samples);
        Add_Branch (theTree, Builder, Top_Node_Cursor);

    end Build_Tree;

    --  ------------------------------------------------------------------

    procedure Init_Tree_Builder
      (Builder               : in out Tree_Builder;
       Splitter              : Node_Splitter.Splitter_Class;
       Min_Samples_Split     : Natural := 0;
       Min_Samples_Leaf      : Natural := 0;
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
