
with Ada.Text_IO; use Ada.Text_IO;

with Classifier_Utilities;

package body Ada_Tree_Build is

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
      Node_Cursor           : Tree.Tree_Cursor := theTree.Nodes.Root;
      Best_Split            : Split_Record;
      Num_Constant_Features : Natural := 0;
--        Num_Node_Samples  : Natural := 0;
   begin
      --  L163
      Node_Splitter.Init (Splitter, X, Y, Sample_Weight);
      Put_Line ("Ada_Tree_Build.Build_Tree Splitter initialized");
      Init_Tree (Builder, Splitter);
      Put_Line ("Ada_Tree_Build.Build_Tree Builder initialized");
      Best_Split := Split_Node (Splitter, Float'Last, Num_Constant_Features);
      Classifier_Utilities.Print_Natural_List
          ("Ada_Tree_Build.Build_Tree Feature_Indices", Splitter.Feature_Indices);
      Classifier_Utilities.Print_Value_List
          ("Ada_Tree_Build.Build_Tree Feature_Values", Splitter.Feature_Values);
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
