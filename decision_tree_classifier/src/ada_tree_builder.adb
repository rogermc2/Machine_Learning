--  Based on scikit-learn/sklearn/tree _tree.pyx L119
--  class DepthFirstTreeBuilder

--  with Ada.Assertions; use Ada.Assertions;
with Ada.Containers;
with Ada.Text_IO; use Ada.Text_IO;

with Build_Utils;
with Node_Splitter;
with Printing;
with Tree;
with Tree_Build;
with Weights;

package body Ada_Tree_Builder is

    Epsilon : constant Float := 10.0 ** (-10);

    First          : Boolean := True;
    Max_Depth_Seen : Positive := 1;

    procedure Init_Tree_Builder
      (Builder               : in out Tree_Builder;
       Splitter              : Node_Splitter.Splitter_Class;
       Min_Samples_Split     : Positive := 2;
       Min_Samples_Leaf      : Positive := 1;
       Min_Weight_Leaf       : Float := 0.0;
       Max_Depth             : Integer := -1;
       Min_Impurity_Decrease : Float := 0.0);

    --  ------------------------------------------------------------------
    --  Based on scikit-learn/sklearn/tree _tree.pyx
    --  DepthFirstTreeBuilder.build
    procedure Add_Branch
      (theTree  : in out Tree.Tree_Class;
       Builder  : in out Tree_Builder;
       theStack : in out Build_Utils.Stack_List;
       Split    : in out Node_Splitter.Split_Record) is
        use Ada.Containers;
        use Build_Utils;
        use Node_Splitter;
        use Tree;
        use Nodes_Package;
        Routine_Name          : constant String :=
                                  "Ada_Tree_Builder.Add_Branch ";
        --  L199
        Data                  : constant Stack_Record := Pop (theStack);
        Start_Row             : constant Positive := Data.Start;
        Stop_Row              : constant Positive := Data.Stop;
        Num_Node_Samples      : constant Positive := Stop_Row - Start_Row + 1;
        Num_Constant_Features : Natural := Data.Num_Constant_Features;
        Is_Leaf_Node          : Boolean := False;
        Impurity              : Float := Float'Last;
        Weighted_Node_Samples : Float := 0.0;
        Values                : Weights.Weight_Lists_2D;
        Child_Cursor          : Tree.Tree_Cursor;
        Node_ID               : Positive := 1;
    begin
        --        Printing.Print_Stack_Record ("Ada_Tree_Builder.Add_Branch stack data",
        --                                     Data);
        --  L209
        --  Reset_Node resets splitter to use samples (Start_Row .. End_Row)
        Reset_Node (Builder.Splitter, Start_Row, Stop_Row,
                    Weighted_Node_Samples);
        --  L216
        Impurity := Data.Impurity;
        Put_Line (Routine_Name & "L214 Impurity: " & Float'Image (Impurity));

        --  L207
        Is_Leaf_Node := Data.Depth >= Builder.Max_Depth or
          Builder.Splitter.Num_Samples = 1 or
          Num_Node_Samples < Builder.Min_Samples_Split or
          Num_Node_Samples < 2 * Builder.Min_Samples_Leaf or
          Weighted_Node_Samples < 2.0 * Builder.Min_Weight_Leaf or
          abs (Impurity) <= Epsilon;  --  0.0 withtolerance for rounding errors

        --  L220
        if not Is_Leaf_Node then
            Split := Split_Node (Builder.Splitter, Impurity,
                                 Num_Constant_Features);
            Printing.Print_Split_Record (Routine_Name & "L221 Split record",
                                         Split);
            --           Put_Line (Routine_Name & ", L220 Split.Split_Row >= Stop_Row: " &
            --                       Boolean'Image (Split.Split_Row >= Stop_Row));
            --           Put_Line (Routine_Name &
            --                       ", L220 Improvement + Epsilon < Min_Impurity_Decrease: " &
            --                       Boolean'Image (Split.Improvement + Epsilon < Builder.Min_Impurity_Decrease));
            --  L233
            Is_Leaf_Node := Split.Split_Row >= Stop_Row or
              Split.Improvement + Epsilon < Builder.Min_Impurity_Decrease;
        end if;

        --  tree.add_node adds one node to the tree
        --  right and left children are added to the stack at
        --  L245 and L251 respectively
        --  L228
        Put_Line (Routine_Name & "L228 Builder Start, Pos, End: " &
                    Integer'Image (Builder.Splitter.Start_Row) & ", " &
                    Integer'Image (Split.Split_Row) & ", " &
                    Integer'Image (Builder.Splitter.Stop_Row));
        if First then
            Child_Cursor := Data.Parent_Cursor;
            First := False;
        else
            Child_Cursor := Tree_Build.Add_Node
              (theTree, Data.Parent_Cursor, Data.Branch, Is_Leaf_Node,
               Split.Feature, Split.Threshold, Impurity, Num_Node_Samples,
               Weighted_Node_Samples);
        end if;
        Node_ID := Element (Child_Cursor).Node_ID;
        Put_Line (Routine_Name & ", L238 child Node_ID: " &
                    Integer'Image (Node_ID));

        --  238
        --  Values dimensions: num outputs x num classes
        Node_Splitter.Node_Value (Builder.Splitter, Values);

        --          Put_Line (Routine_Name & " L238 Node" & Integer'Image (Node_ID));
        --          Printing.Print_Weights_Lists_2D (Routine_Name & " Values", Values);

        if Node_ID > Integer (theTree.Values.Length) then
            theTree.Values.Set_Length (Count_Type (Node_ID));
        end if;
        theTree.Values.Replace_Element (Node_ID, Values);

        --        Put_Line
        --          ("Ada_Tree_Builder.Add_Branch L238 Node_ID, Num_Node_Samples: " &
        --                    Integer'Image (Node_ID) & ", " &
        --                    Integer'Image (Element (Child_Cursor).Num_Node_Samples));
        --  L240
        if not Is_Leaf_Node then
            Put_Line (Routine_Name & "L240 Start, Pos, End: " &
                        Integer'Image (Start_Row) & ", " &
                        Integer'Image (Split.Split_Row) & ", " &
                        Integer'Image (Stop_Row));
            --  Add right branch
            Push (theStack, Split.Split_Row + 1, Stop_Row, Data.Depth + 1,
                  Child_Cursor, Tree.Right_Node, Split.Impurity_Right,
                  Num_Constant_Features);
            --  Add left branch
            Push (theStack, Start_Row, Split.Split_Row, Data.Depth + 1,
                  Child_Cursor, Tree.Left_Node, Split.Impurity_Left,
                  Num_Constant_Features);
        end if;
        --  L254
        if Data.Depth + 1 > Max_Depth_Seen then
            Max_Depth_Seen := Data.Depth + 1;
        end if;

    end Add_Branch;

    --  ------------------------------------------------------------------
    --  L129 DepthFirstTreeBuilder.build
    procedure Build_Tree
      (theTree        : in out Tree.Tree_Class;
       Splitter       : in out Node_Splitter.Splitter_Class;
       X              : ML_Types.Value_Data_Lists_2D;
       Y_Encoded      : Classifier_Types.Natural_Lists_2D;
       Sample_Weights : Weights.Weight_List;
       Max_Depth      : Integer) is
        use Build_Utils;
        use Tree.Nodes_Package;
        use Node_Splitter;
        Routine_Name      : constant String := "Ada_Tree_Builder.Build_Tree ";
        Depth             : constant Natural := 1;
        Builder           : Tree_Builder;
        Start_Row         : constant Positive := 1;
        Stop_Row          : constant Positive := Positive (Y_Encoded.Length);
        Impurity          : Float;
        Constant_Features : Natural := 0;
        Weighted_Samples  : Float := 0.0;
        Stack             : Stack_List;
        Split             : Node_Splitter.Split_Record;
        Values            : Weights.Weight_Lists_2D;
        Top_Node_Cursor   : Cursor;
    begin
        --  L159
        Node_Splitter.Init (Splitter, X, Y_Encoded, Sample_Weights);
        Init_Tree_Builder (Builder, Splitter, Max_Depth => Max_Depth);
        --  L206
        Reset_Node (Builder.Splitter, Start_Row, Stop_Row, Weighted_Samples);
        --  L214
        Impurity := Gini_Node_Impurity (Builder.Splitter);
        Put_Line (Routine_Name & "L214 Impurity first: " &
                    Float'Image (Impurity));
        --  L221 first
        Split := Split_Node (Builder.Splitter, Impurity, Constant_Features);
        Printing.Print_Split_Record (Routine_Name & "L221 first Split record",
                                     Split);
        --  L229 first
        Top_Node_Cursor := Tree_Build.Add_Node
          (theTree, theTree.Nodes.Root, Tree.Top_Node, False, 1, 0.0, Impurity,
           Splitter.Num_Samples, Splitter.Weighted_Samples);
        --  L239 first
        Node_Splitter.Node_Value (Builder.Splitter, Values);
        theTree.Values.Clear;
        theTree.Values.Append (Values);

        --  L184
      Push (Stack, Start_Row, Stop_Row, Depth, Top_Node_Cursor,
            Tree.Left_Node, Impurity, Constant_Features);

        --  L190
        while not Stack.Is_Empty loop
            Add_Branch (theTree, Builder, Stack, Split);
        end loop;

        Put_Line (Routine_Name & "tree built.");
        New_Line;

    end Build_Tree;

    --  ------------------------------------------------------------------

    procedure Init_Tree_Builder
      (Builder               : in out Tree_Builder;
       Splitter              : Node_Splitter.Splitter_Class;
       Min_Samples_Split     : Positive := 2;
       Min_Samples_Leaf      : Positive := 1;
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
        Tree_Build.Reset_Last_Node;

    end Init_Tree_Builder;

    --  ------------------------------------------------------------------

end Ada_Tree_Builder;
