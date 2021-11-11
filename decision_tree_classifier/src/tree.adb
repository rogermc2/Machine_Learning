--  Based on scikit-learn/sklearn/tree _tree.pyx class Tree

with Ada.Assertions; use Ada.Assertions;
--  with Ada.Text_IO; use Ada.Text_IO;

--  with Utilities;

with Classifier_Types;
with Printing;

package body Tree is

   function Apply_Dense (Self : Tree_Class; X : ML_Types.Value_Data_Lists_2D)
                         return Tree_Cursor_List;

   --  -------------------------------------------------------------------------
   --  L770 Apply finds the terminal region (=leaf node) for each sample in X.
   --       That is, the set of relevant nodes containing the prediction values
   --       for each sample?
   function Apply (Self : Tree_Class; X : ML_Types.Value_Data_Lists_2D)
                   return Tree_Cursor_List is
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
   function Apply_Dense (Self : Tree_Class; X : ML_Types.Value_Data_Lists_2D)
                         return Tree_Cursor_List is
      --                           return Classifier_Types.Natural_List is
      --  X is a list of samples of features (num samples x num features)
      use Ada.Containers;
      use ML_Types;
      use Value_Data_Package;
      use Nodes_Package;
      Top_Cursor     : constant Tree_Cursor := First_Child (Self.Nodes.Root);
      Node_Cursor    : Tree_Cursor;
      Node           : Tree_Node;
      Selected_Nodes : Tree_Cursor_List;
      Sample         : Value_Data_List;
      Feature_Value  : Value_Record;
      Use_Left       : Boolean;
   begin
      Assert (Integer (Child_Count (Top_Cursor)) > 0,
              "Tree.Apply_Dense Self.Nodes tree is empty");

      --  L798 for each sample
      for index in X.First_Index .. X.Last_Index loop
         Node_Cursor := Top_Cursor;
         Printing.Print_Node ("Tree.Apply_Dense, top Node", Node_Cursor);
         --  Sample is alist of feature values
         Sample := X.Element (index);

         --  Find a node with a leaf child.
         --  This node has the prediction value.
         while not Element (Node_Cursor).Leaf_Node loop
            Node := Element (Node_Cursor);
            Printing.Print_Node ("Tree.Apply_Dense, Node", Node);
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
            Printing.Print_Node ("Tree.Apply_Dense, Selected child Node",
                                 Node_Cursor);
         end loop;  --  Not_Leaf

         Printing.Print_Node ("Tree.Apply_Dense, Selected_Node", Node_Cursor);
         Selected_Nodes.Append (Node_Cursor);
      end loop;

      return Selected_Nodes;

   end Apply_Dense;

   --  ------------------------------------------------------------------------
   --  _tree L758
   function Predict (Self : in out Tree_Class;
                     X    : ML_Types.Value_Data_Lists_2D)
                     return Weights.Weight_Lists_3D is
      --  X is a list of samples
      --  Each sample is a list of feature values, one value per feature
      use Weights;
      Selected_Nodes  : Tree_Cursor_List;
      Node_Cursor     : Tree_Cursor;
      Node            : Tree_Node;
      Out_Data        : Weight_Lists_3D;
   begin
      Printing.Print_Value_Data_Lists_2D ("Tree.Predict, X ", X);
      --  L760;
      Selected_Nodes := Apply (Self, X);
      Printing.Print_Weight_Lists_3D ("Tree.Predict, Self.Values", Self.Values);
      Printing.Print_Node_Cursor_List ("Tree.Predict, Apply: Selected_Nodes",
                                       Selected_Nodes);
      for index in Selected_Nodes.First_Index .. Selected_Nodes.Last_Index loop
         Node_Cursor := Selected_Nodes.Element (index);
         Printing.Print_Node ("Tree.Predict, Selected_Node", Node_Cursor);
         Node := Element (Node_Cursor);
         Out_Data.Append (Self.Values.Element (Node.Node_ID));
      end loop;

      Printing.Print_Weight_Lists_3D ("Tree.Predict, Out_Data", Out_Data);
      return Out_Data;

   end Predict;

   --  -------------------------------------------------------------------------

end Tree;
