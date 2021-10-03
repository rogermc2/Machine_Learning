
with Ada.Text_IO; use Ada.Text_IO;

with Classifier_Types;
with Classifier_Utilities;
with Node_Splitter;
with Tree;
with Tree_Build;

package body Ada_Tree_Build is

    Epsilon : constant Float := 10.0 ** (-10);

    First          : Boolean := True;
    Max_Depth_Seen : Natural := 0;

    --     procedure Add_Decision_Node (theTree       : in out Tree.Tree_Class;
    --                                  Parent_Cursor : Tree.Tree_Cursor;
    --                                  Best_Split    : Node_Splitter.Split_Record) ;
    --     procedure Add_Prediction_Node (theTree       : in out Tree.Tree_Class;
    --                                    Parent_Cursor : Tree.Tree_Cursor;
    --                                    Start, Stop   : Natural);
    procedure Init_Tree_Builder
      (Builder               : in out Tree_Builder;
       Splitter              : Node_Splitter.Splitter_Class;
       Min_Samples_Split     : Natural := 0;
       Min_Samples_Leaf      : Natural := 0;
       Min_Weight_Leaf       : Float := 0.0;
       Max_Depth             : Natural := 0;
       Min_Impurity_Decrease : Float := 0.0);

    --  ------------------------------------------------------------------

    procedure Add_Branch
      (theTree               : in out Tree.Tree_Class;
       Builder               : in out Tree_Builder;
       Start, Stop           : in out Natural;
       Num_Constant_Features : in out Natural;
       Parent_Cursor         : in out Tree.Tree_Cursor) is
    --  Parent_Cursor is a cursor to an existing node which is the head
    --  of this branch
        use Node_Splitter;
        use Tree;
        use Nodes_Package;
        use Values_Package;
        use Output_Package;
        use Class_Package;
        Splitter              : Node_Splitter.Splitter_Class :=
                                  Builder.Splitter;
        Split                 : Split_Record;
        --  Parent_Node corresponds to popped stack_record?
        --  L199
        Parent_Node           : constant Tree.Tree_Node := Element (Parent_Cursor);
        Num_Node_Samples      : constant Natural := Stop - Start;  --  L207
        Is_Leaf               : Boolean := False;
        Impurity              : Float := Float'Last;
        Weighted_Node_Samples : Float := 0.0;
        Classes               : Class_List;
        Outputs               : Output_List;
        Value                 : Classifier_Types.Float_List;
        --  A Values_List is a list of Output_Lists of Class_Lists of Floats;
        Depth                 : Natural := Parent_Node.Depth;
        Child_Cursor          : Tree.Tree_Cursor;
        Output_Index          : Positive := 1;
        Class_Index           : Positive := 1;
    begin
        --  L208
        --  Reset_Node resets splitter to use samples (Start .. Stop)
        Put_Line ("Ada_Tree_Build.Add_Branch Reset_Node");
        Reset_Node (Splitter, Start, Stop, Weighted_Node_Samples);
        if First then
            Impurity := Node_Impurity (Splitter);
            First := False;
        end if;
        Put_Line ("Ada_Tree_Build.Add_Branch First done");

        --  L210
        Is_Leaf := Depth >= Builder.Max_Depth or
          Num_Node_Samples < Builder.Min_Samples_Split or
          Num_Node_Samples < 2 * Builder.Min_Samples_Leaf or
          Weighted_Node_Samples < 2.0 * Builder.Min_Weight_Leaf or
          --  if Impurity == 0.0 with tolerance for rounding errors
          Impurity <= Epsilon;

        --  L222
        if not Is_Leaf then
            Split := Split_Node (Splitter, Impurity, Num_Constant_Features);
            Is_Leaf := Split.Pos_I >= Stop or
              Split.Improvement + Epsilon <= Builder.Min_Impurity_Decrease;
        end if;
        --  L229
        Child_Cursor := Tree_Build.Add_Node
          (theTree, Depth, Parent_Cursor, True, Is_Leaf, Split.Feature_Index,
           Impurity, Split.Threshold, Weighted_Node_Samples);

        --  L237 Store values for all nodes to facilitate tree/model
        --  inspection and interpretation
        Node_Value (Splitter, Value);
        --  Update theTree.Values
        Outputs := theTree.Values.Element (Output_Index);
        Classes.Clear;
        for index in Value.First_Index .. Value.Last_Index loop
            Classes.Append  (Value.Element (index));
        end loop;
        Outputs.Replace_Element (Class_Index, Classes);
        theTree.Values.Replace_Element (Output_Index, Outputs);

        --  L241 Nodes already added by Tree_Build.Add_Node

        --  L254
        if Depth > Max_Depth_Seen then
            Max_Depth_Seen := Depth;
        end if;

        if not Is_Leaf then
            Add_Branch (theTree, Builder, Start, Stop, Num_Constant_Features,
                        Child_Cursor);
        end if;

        --        if Split.Improvement = 0.0 then
        --           --  L357?
        --           Add_Prediction_Node (theTree, Parent_Cursor, Start, Stop);
        --        else
        --           Split := Node_Splitter.Split_Node (Splitter, Parent_Impurity,
        --                                              Constant_Features);
        --           Is_Leaf := Split.Pos_I >= Stop or
        --             Split.Improvement + Epsilon < Builder.Min_Impurity_Decrease;
        --           Child_Cursor :=
        --             Tree_Build.Add_Node (theTree, Parent_Cursor, Is_Left, Is_Leaf,
        --                                  Feature_Index, Split.Impurity_Left, Split.Threshold,
        --                                  Weighted_Node_Samples);
        --
        --           Add_Decision_Node (theTree, Parent_Cursor, Split);
        --           Child_Cursor := Last_Child (Parent_Cursor);
        --           Add_Branch (theTree, Builder, Start, Stop,
        --                       Num_Constant_Features, Child_Cursor);
        --           Add_Branch (theTree, Builder, Start, Stop,
        --                       Num_Constant_Features, Child_Cursor);
        --        end if;

    end Add_Branch;

    --  ------------------------------------------------------------------

    procedure Add_Decision_Node (theTree       : in out Tree.Tree_Class;
                                 Parent_Cursor : Tree.Tree_Cursor;
                                 Best_Split    : Node_Splitter.Split_Record) is
        use Tree;
        use Tree.Nodes_Package;
        Node  : Tree_Node;
    begin
        --        Node.Question := Best_Split.Question;
        --        Node.True_Branch := Best_Split.True_Rows;
        --        Node.False_Branch := Best_Split.False_Rows;
        Node.Impurity := Best_Split.Improvement;
        theTree.Nodes.Insert_Child (Parent   => Parent_Cursor,
                                    Before   => No_Element,
                                    New_Item => Node);
    end Add_Decision_Node;

    --  ------------------------------------------------------------------

    procedure Add_Prediction_Node (theTree       : in out Tree.Tree_Class;
                                   Parent_Cursor : Tree.Tree_Cursor;
                                   Start, Stop   : Natural) is
        use Tree;
        use Nodes_Package;
        Leaf : Tree_Node (Is_Leaf => True);
    begin
        --        if Max_Leaves > 0 then
        --           Num_Leaves := Num_Leaves + 1;
        --        end if;
        --           New_Line;
        Leaf.Samples_Start := Start;
        Leaf.Samples_End := Stop;

        theTree.Nodes.Insert_Child (Parent_Cursor, No_Element, Leaf);
    end Add_Prediction_Node;

    --  ------------------------------------------------------------------

    procedure Build_Tree
      (theTree       : in out Tree.Tree_Class;
       X, Y          : ML_Types.List_Of_Value_Data_Lists;
       Sample_Weight : Weights.Weight_List) is
       use Tree.Nodes_Package;
        use Node_Splitter;
        Builder               : Tree_Builder;
        Splitter              : Splitter_Class;
        Num_Constant_Features : Natural := 0;
        Start                 : Positive := 1;
        Stop                  : Positive := X.Element (1).Last_Index;
        Top_Node              : Tree.Tree_Node;
        Top_Node_Cursor       : Cursor;
    begin
        --  L163
        Put_Line ("Ada_Tree_Build.Build_Tree Y size" &
                    Integer'Image (Integer (Y.Length)) & " x " &
                    Integer'Image (Integer (Y.Element (1).Length)));
        Node_Splitter.Init (Splitter, X, Y, Sample_Weight);
        Init_Tree_Builder (Builder, Splitter);
        Put_Line ("Ada_Tree_Build.Build_Tree Builder initialized");
        Put_Line ("Ada_Tree_Build.Build_Tree Y size" &
                    Integer'Image (Integer (Y.Length)) & " x " &
                    Integer'Image (Integer (Y.Element (1).Length)));

        Classifier_Utilities.Print_Natural_List
          ("Ada_Tree_Build.Build_Tree Feature_Indices", Splitter.Feature_Indices);
        Classifier_Utilities.Print_Value_List
          ("Ada_Tree_Build.Build_Tree Feature_Values", Splitter.Feature_Values);
        Top_Node.Impurity := Splitter.Node_Impurity;
        theTree.Nodes.Prepend_Child (theTree.Nodes.Root, Top_Node);
        Top_Node_Cursor := Last_Child (theTree.Nodes.Root);
        Add_Branch (theTree, Builder, Start, Stop,
                    Num_Constant_Features, Top_Node_Cursor);

    end Build_Tree;

    --  ------------------------------------------------------------------

    procedure Init_Tree_Builder
      (Builder               : in out Tree_Builder;
       Splitter              : Node_Splitter.Splitter_Class;
       Min_Samples_Split     : Natural := 0;
       Min_Samples_Leaf      : Natural := 0;
       Min_Weight_Leaf       : Float := 0.0;
       Max_Depth             : Natural := 0; --  -1 means undefined
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

end Ada_Tree_Build;
