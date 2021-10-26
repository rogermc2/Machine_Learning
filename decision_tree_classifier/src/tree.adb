--  Based on scikit-learn/sklearn/tree _tree.pyx class Tree

with Ada.Assertions; use Ada.Assertions;
--  with Ada.Text_IO; use Ada.Text_IO;

--  with Utilities;

--  with Printing;

package body Tree is

   function Apply_Dense (Self : Tree_Class;
                         X    : ML_Types.Value_Data_Lists_2D)
                          return Tree_Cursor_List;
   procedure Save_Nodes (aTree   : in out Tree_Class;
                         Cursors : Tree_Cursor_List);

   --  -------------------------------------------------------------------------
   --  L770 Apply finds the terminal region (=leaf node) for each sample in X.
   function Apply (Self : Tree_Class;
                   X    : ML_Types.Value_Data_Lists_2D)
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
   function Apply_Dense (Self : Tree_Class;
                         X    : ML_Types.Value_Data_Lists_2D)
                          return Tree_Cursor_List is
      --  X is a list of samples of features
      use Ada.Containers;
      use ML_Types;
      use Nodes_Package;
      Top_Cursor     : constant Tree_Cursor := First_Child (Self.Nodes.Root);
      Node_Cursor    : Tree_Cursor;
      Node           : Tree_Node;
      Sample         : Value_Data_List;
      Feature_Value  : Value_Record;
      Use_Left       : Boolean;
      Output_Cursors : Tree_Cursor_List;
   begin
      Assert (Integer (Child_Count (Top_Cursor)) > 0,
              "Tree.Apply_Dense Self.Nodes tree is empty");

      --  L804 for each sample of features
      for index in X.First_Index .. X.Last_Index loop
         Node_Cursor := Top_Cursor;
         Sample := X.Element (index);

--           while not Element (First_Child (Node_Cursor)).Leaf_Node loop
         while not Element (Node_Cursor).Leaf_Node loop
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

         Output_Cursors.Append (Node_Cursor);
      end loop;

      return Output_Cursors;

   end Apply_Dense;

   --  -------------------------------------------------------------------------
   --  _tree L763
   function Predict (Self : in out Tree_Class;
                     X    : ML_Types.Value_Data_Lists_2D)
                      return Weights.Weight_Lists_3D is
      --                       return ML_Types.Value_Data_Lists_3D is
      --  X is a list of samples
      --  Each sample is a list of feature values, one value per feature
      --  Leaf_Cursors is a list of leaf node cursors, one for each sample
      Leaf_Cursors  : Tree_Cursor_List;
      Cursor_ID     : Natural := 0;
      Output_Values : Weights.Weight_Lists_3D;

      --  L1087 node dimension of _get_value_ndarray?
      --  The predicted class probability is the fraction of samples
      --   of the same class in a leaf.
      procedure Build_Output (Curs : Tree_Cursor_Package.Cursor) is
         use Tree_Cursor_Package;
         use Nodes_Package;
         Node         : constant Tree_Node := Element (Element (Curs));
         Values       : constant Weights.Weight_Lists_2D := Node.Values;
         Classes_Out  : Weights.Weight_Lists_2D;
         Class_Values : Weights.Weight_List;
      begin
         Cursor_ID := Cursor_ID + 1;
         Assert (not Values.Is_Empty, "Tree.Predict.Build_Output Cursor ID" &
                   Integer'Image (Cursor_ID) & " Values list is empty");
--           Printing.Print_Weights_Lists
--             ("Tree.Predict Build_Output Values for Cursor ID" &
--              Integer'Image (Cursor_ID), Values);
         for output_index in Values.First_Index .. Values.Last_Index loop
            Class_Values := Values.Element (output_index);
            Classes_Out.Append (Class_Values);
         end loop;
         Output_Values.Append (Classes_Out);
      end Build_Output;

   begin
      --  L767
      Leaf_Cursors := Apply (Self, X);
--        Printing.Print_Node_Cursor_List ("Tree.Predict Leaf_Cursors",
--                                         Leaf_Cursors);
      Save_Nodes (Self, Leaf_Cursors);
      Leaf_Cursors.Iterate (Build_Output'access);
      --        Printing.Print_Weight_Lists_3D
      --          ("Tree.Predict Output Values...", Output_Values);

      return Output_Values;

   end Predict;

   --  -------------------------------------------------------------------------

   procedure Save_Nodes (aTree   : in out Tree_Class;
                         Cursors : Tree_Cursor_List) is
      use Nodes_List_Package;
   begin
      aTree.Values.Clear;
      for index in Cursors.First_Index .. Cursors.Last_Index loop
         aTree.Values.Append (Element (Cursors (index)));
      end loop;

   end Save_Nodes;

   --  -------------------------------------------------------------------------

end Tree;
