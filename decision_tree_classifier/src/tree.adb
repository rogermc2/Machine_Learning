--  Based on scikit-learn/sklearn/tree _tree.pyx class Tree

with Ada.Assertions; use Ada.Assertions;
--  with Ada.Text_IO; use Ada.Text_IO;

with Classifier_Types;
with Printing;

package body Tree is

   function Apply_Dense (Self : Tree_Class; X : ML_Types.Value_Data_Lists_2D)
                         return Classifier_Types.Natural_List;

   --  -------------------------------------------------------------------------
   --  L770 Apply finds the terminal region (=leaf node) for each sample in X.
   --       That is, the set of relevant nodes containing the prediction values
   --       for each sample?
   --  Apply returns a list containing the Node ID associated with each sample.
   function Apply (Self : Tree_Class; X : ML_Types.Value_Data_Lists_2D)
                   return Classifier_Types.Natural_List is
   begin
      return Apply_Dense (Self, X);
   end Apply;

   --  -------------------------------------------------------------------------
   --  L777
   --  Apply_Dense finds the terminal region (=leaf node) for each sample in X.
   --  Through splitting, different subsets of a dataset are created with each
   --  instance belonging to one subset.
   --  The final subsets are called terminal or leaf nodes and the
   --  intermediate subsets are called internal nodes or split nodes.
   --  Apply_Dense returns a list containing the Node ID associated with each
   --  sample
   function Apply_Dense (Self : Tree_Class; X : ML_Types.Value_Data_Lists_2D)
                         return Classifier_Types.Natural_List is
      --                           return Tree_Cursor_List is
      --  X is a list of samples of features (num samples x num features)
      use Ada.Containers;
      use ML_Types;
      use Value_Data_Package;
      use Nodes_Package;
--        Routine_Name : constant String := "Tree.Apply_Dense ";
      Top_Cursor     : constant Tree_Cursor := First_Child (Self.Nodes.Root);
      Num_Samples    : constant Positive := Positive (X.Length);
      Node_Cursor    : Tree_Cursor;
      Node           : Tree_Node;
      Sample         : Value_Data_List;
      Feature_Value  : Value_Record;
      Out_Data       : Classifier_Types.Natural_List;
      Use_Left       : Boolean;
   begin
      --  L790
      for index in 1 .. Num_Samples loop
         Out_Data.Append (0);
      end loop;

      --  L798 for each sample
      for index in X.First_Index .. X.Last_Index loop
         Node_Cursor := Top_Cursor;
         --  Sample is a list of feature values
         Sample := X.Element (index);

         if Integer (Child_Count (Top_Cursor)) > 0 then
            --  Find a node with a leaf child.
            --  This node has the prediction value.
            while not Element (First_Child (Node_Cursor)).Leaf_Node loop
               Node := Element (Node_Cursor);
               Assert (Feature_Value.Value_Kind = Float_Type or
                         Feature_Value.Value_Kind = Integer_Type,
                       "Tree.Apply_Dense Self.Nodes invalid feature data type");
               Feature_Value := Sample.Element (Node.Best_Fit_Feature_Index);
               --  Make tree traversal decision
               case Feature_Value.Value_Kind is
                  when Float_Type =>
                     Use_Left := Feature_Value.Float_Value <= Node.Threshold;
                  when Integer_Type =>
                     Use_Left := Float (Feature_Value.Integer_Value) <=
                       Node.Threshold;
                  when others => null;
               end case;

               if Use_Left then
                  Node_Cursor := First_Child (Node_Cursor);
               else
                  Node_Cursor := Last_Child (Node_Cursor);
               end if;

               Out_Data.Replace_Element (index, Element (Node_Cursor).Node_ID);
            end loop;  --  Not_Leaf
         else
            Out_Data.Replace_Element (index, Element (Node_Cursor).Node_ID);
         end if;
      end loop;

      return Out_Data;

   end Apply_Dense;

   --  ------------------------------------------------------------------------

   procedure C_Init (aTree        : in out Tree_Class; Num_Features : Natural := 0;
                     Num_Classes  : Classifier_Types.Natural_List :=
                       Classifier_Types.Natural_Package.Empty_Vector;
                     Num_Outputs  : Index_Range := 1) is
   begin
      aTree.Num_Features := Num_Features;
      aTree.Num_Classes := Num_Classes;
      aTree.Num_Outputs := Num_Outputs;
      aTree.Max_Depth := 0;
      atree.Values := Weights.Weight_Lists_3D_Package.Empty_Vector;
      aTree.Nodes := Nodes_Package.Empty_Tree;
      aTree.Classes := ML_Types.Value_Lists_Data_Package.Empty_Vector;

   end C_Init;

   --  ------------------------------------------------------------------------
   --  _tree L758
   --  Predict returns a 3D list, num_samples x num_outputs x num_classes
   function Predict (Self : in out Tree_Class;
                     X    : ML_Types.Value_Data_Lists_2D)
                     return Weights.Weight_Lists_3D is
      use Ada.Containers;
      Routine_Name : constant String := "Tree.Predict ";
      --  X is a list of samples
      --  Each sample is a list of feature values, one value per feature
      --  Values: num_nodes x num_outputs x num_classes
      Values       : constant Weights.Weight_Lists_3D := Self.Values;
      Samples      : Classifier_Types.Natural_List;
      --  Out_Data: num_samples x num_outputs x num_classes
      Out_Data     : Weights.Weight_Lists_3D;
   begin
      --  Check for top node, child of root node
      Assert (Self.Nodes.Node_Count > 1, Routine_Name & " Tree is empty.");

      --  L760  Apply returns a list containing the Node ID associated with
      --        each sample.
      Samples := Apply (Self, X);
      Printing.Print_Natural_List (Routine_Name & " samples", Samples);
      for index in Samples.First_Index .. Samples.Last_Index loop
         if Samples.Element (index) > 0 then
            Out_Data.Append (Values.Element (Samples.Element (index)));
         end if;
      end loop;
--        Put_Line (Routine_Name & ", samples size" &
--                    Count_Type'Image (Samples.Length));
      return Out_Data;

   end Predict;

   --  -------------------------------------------------------------------------

end Tree;
