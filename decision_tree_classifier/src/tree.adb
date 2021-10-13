--  Based on scikit-learn/sklearn/tree _tree.pyx class Tree

with Ada.Text_IO; use Ada.Text_IO;

package body Tree is

   function Apply_Dense (Self : Tree_Class;
                         X    : ML_Types.List_Of_Value_Data_Lists)
                         return Leaf_Cursor_Array;

   --  -------------------------------------------------------------------------
   --  Apply finds the terminal region (=leaf node) for each sample in X.
   function Apply (Self : Tree_Class;
                   X    : ML_Types.List_Of_Value_Data_Lists)
                   return Leaf_Cursor_Array is
   begin
      return Apply_Dense (Self, X);
   end Apply;

   --  -------------------------------------------------------------------------

   function Apply_Dense (Self : Tree_Class;
                         X    : ML_Types.List_Of_Value_Data_Lists)
                         return Leaf_Cursor_Array is
      use ML_Types;
      use Nodes_Package;
      Node_Cursor  : Tree_Cursor;
      Node         : Tree_Node;
      Feature      : Positive;
      Samples      : Value_Data_List;
      Leaf_Cursors : Leaf_Cursor_Array (1 .. Positive (X.Length));
   begin
      if Integer (Child_Count (Self.Nodes.Root)) = 0 then
         raise Value_Error with
           "Tree.Apply_Dense Self.Nodes tree is empty";
      end if;

      for index in X.First_Index .. X.Last_Index loop
         Samples := X.Element (index);
         Node_Cursor := First_Child (Self.Nodes.Root);
         while Has_Element (Node_Cursor) and then
           not Element (Node_Cursor).Is_Leaf loop
            Node := Element (Node_Cursor);
            Feature := Node.Feature_Index;
            if Node.Is_Leaf and then Samples.Element (Feature).Float_Value <= Node.Threshold then
               Node_Cursor := First_Child (Node_Cursor);
            else
               Node_Cursor := Last_Child (Node_Cursor);
            end if;
         end loop;
         Leaf_Cursors (index) := Node_Cursor;
      end loop;
      New_Line;
      return Leaf_Cursors;

   end Apply_Dense;

   --  -------------------------------------------------------------------------

--     function Get_Value_Array (Self : Tree_Class) return Value_Array is
--        Values : Value_Array (1 .. Positive (Self.Values.Length));
--        Values_Data : Values_List;
--     begin
--        for v_index in Values_Data.First_Index .. Values_Data.Last_Index loop
--           Values (v_index) := Values_Data.Element (v_index);
--        end loop;
--
--        return Values;
--
--     end Get_Value_Array;

   --  -------------------------------------------------------------------------

   function Predict (Self : Tree_Class;
                     X    : ML_Types.List_Of_Value_Data_Lists)
                     return ML_Types.Value_Data_List is
      --  X is a list of samples
      --  Each sample is a list of feature values, one value per feature
      use ML_Types;
      N_Samples       : constant Natural := Natural (X.Length);
      --  Apply finds the terminal region (=leaf node) for each sample in X.
      --  Leaf_Cursors is a list of feature cursors, each cursor corresponding to
      --  a leaf noode of X
      --        Leaf_Values  : constant Value_Array
      --          (1 .. Self.Node_Count, 1 .. Self.Num_Outputs, 1 .. Self.Num_Features)
      --          := Get_Value_Array (Self);
      --        Axis_0       : array (1 .. Self.Num_Outputs, 1 .. Self.Num_Features)
      --          of Float;
      Leaf_Cursors    : Leaf_Cursor_Array (1 .. N_Samples);
      Leaf            : Tree_Node;
      Feature_Index   : Positive;
      Features_List   : Value_Data_List;
      Target          : Value_Data_List;
   begin
      if Integer (Child_Count (Self.Nodes.Root)) = 0 then
         raise Value_Error with
           "Tree.Predict Self.Nodes tree is empty";
      end if;
      Leaf_Cursors := Apply (Self, X);

      for index in 1 .. N_Samples loop
         Leaf := Element (Leaf_Cursors (index));
         if not Leaf.Is_Leaf then
            Feature_Index := Leaf.Feature_Index;
            Features_List := X.Element (index);
            Target.Append (Features_List (Feature_Index));
         end if;
      end loop;

      return Target;

   end Predict;

   --  -------------------------------------------------------------------------

end Tree;
