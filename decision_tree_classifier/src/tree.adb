--  Based on scikit-learn/sklearn/tree _tree.pyx class Tree

with Ada.Assertions; use Ada.Assertions;
--  with Ada.Text_IO; use Ada.Text_IO;

--  with Utilities;

--  with Printing;

package body Tree is

   function Apply_Dense (Self           : Tree_Class;
                         X              : ML_Types.Value_Data_Lists_2D;
                         Selected_Nodes : out Tree_Cursor_List)
                          return ML_Types.Value_Data_List;

   --  -------------------------------------------------------------------------
   --  L770 Apply finds the terminal region (=leaf node) for each sample in X.
   --       That is, the set of relevant nodes containing the prediction values
   --       for each sample?
   function Apply (Self           : Tree_Class;
                   X              : ML_Types.Value_Data_Lists_2D;
                   Selected_Nodes : Out Tree_Cursor_List)
                    return ML_Types.Value_Data_List is
   begin
      return Apply_Dense (Self, X, Selected_Nodes);
   end Apply;

   --  -------------------------------------------------------------------------
   --  L777
   --  Apply_Dense finds the terminal region (=leaf node) for each sample in X.
   --  Through splitting, different subsets of a dataset are created with each
   --  instance belonging to one subset.
   --  The final subsets are called terminal or leaf nodes and the
   --  intermediate subsets are called internal nodes or split nodes.
   function Apply_Dense (Self           : Tree_Class;
                         X              : ML_Types.Value_Data_Lists_2D;
                         Selected_Nodes : out Tree_Cursor_List)
                          return ML_Types.Value_Data_List is
      --  X is a list of samples of features
      use Ada.Containers;
      use ML_Types;
      use Value_Data_Package;
      use Nodes_Package;
      Top_Cursor     : constant Tree_Cursor := First_Child (Self.Nodes.Root);
      Node_Cursor    : Tree_Cursor;
      Node           : Tree_Node;
      Out_Data       : Value_Data_List;
      Sample         : Value_Data_List;
      Feature_Value  : Value_Record;
      Use_Left       : Boolean;
   begin
      Assert (Integer (Child_Count (Top_Cursor)) > 0,
              "Tree.Apply_Dense Self.Nodes tree is empty");
      Selected_Nodes.Clear;
      --  L804 for each sample of features
      for index in X.First_Index .. X.Last_Index loop
         Node_Cursor := Top_Cursor;
         Sample := X.Element (index);

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
         end loop;  --  Not_Leaf

         Feature_Value :=
           Sample.Element (Element (Node_Cursor).Best_Fit_Feature_Index);
         Out_Data.Append (Feature_Value);
         Selected_Nodes.Append (Node_Cursor);
      end loop;

      return Out_Data;

   end Apply_Dense;

   --  ------------------------------------------------------------------------
   --  _tree L763
   function Predict (Self : in out Tree_Class;
                     X    : ML_Types.Value_Data_Lists_2D)
                      return ML_Types.Value_Data_List is
      --  X is a list of samples
      --  Each sample is a list of feature values, one value per feature
      --  Leaf_Cursors is a list of leaf node cursors, one for each sample
      Selected_Nodes  : Tree_Cursor_List;
      Out_Data        : ML_Types.Value_Data_List;

   begin
      --  L767;
      Out_Data := Apply (Self, X, Selected_Nodes);
      --        Printing.Print_Node_Cursor_List ("Tree.Predict Selected_Nodes",
      --                                         Selected_Nodes);

      return Out_Data;

   end Predict;

   --  -------------------------------------------------------------------------

end Tree;
