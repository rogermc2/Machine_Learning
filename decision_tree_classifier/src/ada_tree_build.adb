
with Ada.Text_IO; use Ada.Text_IO;

with Classifier_Utilities;

with Node_Splitter;
with Tree;

package body Ada_Tree_Build is

   procedure Add_Decision_Node (theTree       : in out Tree.Tree_Type;
                                Parent_Cursor : Tree.Tree_Cursor;
                                Best_Split    : Node_Splitter.Split_Record) ;
   procedure Add_Prediction_Node (theTree       : in out Tree.Tree_Type;
                                  Parent_Cursor : Tree.Tree_Cursor;
                                  Start, Stop   : in out Natural);

   --  ------------------------------------------------------------------

   procedure Add_Branch (theTree               : in out Tree.Tree_Type;
                         Splitter              : in out Node_Splitter.Splitter_Class;
                         Start, Stop           : in out Natural;
--                           X, Y                  : ML_Types.List_Of_Value_Data_Lists;
                         Num_Constant_Features : in out Natural;
                         Parent_Cursor         : Tree.Tree_Cursor) is
      --  Parent_Cursor is a cursor to an existing node which is the head
      --  of this branch
      use Node_Splitter;
      use Tree.Tree_Package;
      Best_Split    : constant Split_Record
        := Split_Node (Splitter, Float'Last, Num_Constant_Features);
      --  Parent_Node corresponds to popped stack_record?
      --  L199
      Parent_Node       : constant Tree.Tree_Node := Element (Parent_Cursor);
--        Start             : constant Natural := Parent_Node.Samples_Start;
--        Stop              : constant Natural := Parent_Node.Samples_End;
      Parent_Is_Left    : constant Boolean := Parent_Node.Is_Left;
      Parent_Impurity   : constant Float := Parent_Node.Impurity;
      Constant_Features : Integer := Parent_Node.Num_Constant_Features;
      Num_Node_Samples  : constant Natural := Stop - Start;
      Weighted_Node_Samples : Float := 0.0;
      Child_Cursor : Tree.Tree_Cursor;
   begin
      --  L208
      Reset_Node (Splitter, Start, Stop, Weighted_Node_Samples);
      if Best_Split.Improvement = 0.0 then
         Add_Prediction_Node (theTree, Parent_Cursor, Start, Stop);
      else
         Add_Decision_Node (theTree, Parent_Cursor, Best_Split);
         Child_Cursor := Last_Child (Parent_Cursor);
         Add_Branch (theTree, Splitter, Start, Stop,
                     Num_Constant_Features, Child_Cursor);
         Add_Branch (theTree, Splitter, Start, Stop,
                     Num_Constant_Features, Child_Cursor);
      end if;
   end Add_Branch;

   --  ------------------------------------------------------------------

   procedure Add_Decision_Node (theTree       : in out Tree.Tree_Type;
                                Parent_Cursor : Tree.Tree_Cursor;
                                Best_Split    : Node_Splitter.Split_Record) is
      use Tree;
      use Tree.Tree_Package;
      Node  : Tree_Node;
   begin
--        Node.Question := Best_Split.Question;
--        Node.True_Branch := Best_Split.True_Rows;
--        Node.False_Branch := Best_Split.False_Rows;
      Node.Impurity := Best_Split.Improvement;
      theTree.Insert_Child (Parent   => Parent_Cursor,
                            Before   => No_Element,
                            New_Item => Node);
   end Add_Decision_Node;

   --  ------------------------------------------------------------------

   procedure Add_Prediction_Node (theTree       : in out Tree.Tree_Type;
                                  Parent_Cursor : Tree.Tree_Cursor;
                                  Start, Stop   : in out Natural) is
      use Tree;
      use Tree_Package;
      Leaf : Tree_Node (Is_Leaf => True);
   begin
--        if Max_Leaves > 0 then
--           Num_Leaves := Num_Leaves + 1;
--        end if;
      --           New_Line;
      Leaf.Samples_Start := Start;
      Leaf.Samples_End := Stop;

      theTree.Insert_Child (Parent_Cursor, No_Element, Leaf);
   end Add_Prediction_Node;

   --  ------------------------------------------------------------------

   procedure Init_Tree
     (Builder               : in out Tree_Builder;
      Splitter              : Node_Splitter.Splitter_Class;
      Min_Samples_Split     : Natural := 0;
      Min_Samples_Leaf      : Natural := 0;
      Min_Weight_Leaf       : Float := 0.0;
      Max_Depth             : Natural := 0;
      Min_Impurity_Decrease : Float := 0.0);

   --  ------------------------------------------------------------------

   procedure Build_Tree
     (theTree       : in out Tree.Tree_Class;
      X, Y          : ML_Types.List_Of_Value_Data_Lists;
      Sample_Weight : Classifier_Types.Weight_List) is
      use Node_Splitter;
      Builder               : Tree_Builder;
      Splitter              : Splitter_Class;
      Num_Constant_Features : Natural := 0;
      Impurity              : Float;
   begin
      --  L163
      Node_Splitter.Init (Splitter, X, Y, Sample_Weight);
      Init_Tree (Builder, Splitter);
      Put_Line ("Ada_Tree_Build.Build_Tree Builder initialized");

      Classifier_Utilities.Print_Natural_List
        ("Ada_Tree_Build.Build_Tree Feature_Indices", Splitter.Feature_Indices);
      Classifier_Utilities.Print_Value_List
        ("Ada_Tree_Build.Build_Tree Feature_Values", Splitter.Feature_Values);
      Impurity := Splitter.Node_Impurity;
      Add_Branch (Splitter, 1, X.Element(1).Last_Index, theTree.Nodes.Root, Num_Constant_Features);
   end Build_Tree;

   --  ------------------------------------------------------------------

   procedure Init_Tree
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
   end Init_Tree;

   --  ------------------------------------------------------------------

end Ada_Tree_Build;
