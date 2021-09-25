
package body Ada_Tree_Build is

   procedure Build_Tree
     (Builder       : in out Tree_Builder;
      theTree       : in out Tree.Tree_Class;
      X, Y          : ML_Types.List_Of_Value_Data_Lists;
      Sample_Weight : Classifier_Types.Weight_List) is
      Splitter          : Node_Splitter.Splitter_Class;
      Node_Cursor       : Tree.Tree_Cursor := theTree.Nodes.Root;
--        Num_Node_Samples  : Natural := 0;
   begin
      --  L163
      Node_Splitter.Init (Splitter, X, Y, Sample_Weight);
--        Num_Node_Samples := Splitter.Num_Samples;
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
