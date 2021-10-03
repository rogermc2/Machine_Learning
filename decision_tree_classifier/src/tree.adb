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
         Put_Line ("Tree.Apply_Dense index" & Integer'Image (index));
         Samples := X.Element (index);
         Put_Line ("Tree.Apply_Dense Samples set");
         Node_Cursor := First_Child (Self.Nodes.Root);
         Put_Line ("Tree.Apply_Dense Node_Cursor set");
         Put_Line ("Tree.Apply_Dense Node_Cursor Has_Element; " &
                     Boolean'Image (Has_Element (Node_Cursor)));
         while Has_Element (Node_Cursor) and then
           not Element (Node_Cursor).Is_Leaf loop
            Put_Line ("Tree.Apply_Dense not leaf");
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

   function Get_Value_Array (Self : Tree_Node) return Value_Array is
      Values : Value_Array (1 .. Positive (Self.Values.Length));
      Values_Data : Values_List;
   begin
      for v_index in Values_Data.First_Index .. Values_Data.Last_Index loop
         Values (v_index) := Values_Data.Element (v_index);
      end loop;

      return Values;

   end Get_Value_Array;

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
      --        for o_index in 1 .. Self.Num_Outputs loop
      --           for c_index in 1 .. Self.Num_Features loop
      --              Axis_0 (o_index, c_index) :=
      --                Leaf_Cursors (1, o_index, c_index);
      --           end loop;
      --        end loop;

      for index in 1 .. N_Samples loop
         Put_Line ("Tree.Predict index" & Integer'Image (index));
         Leaf := Element (Leaf_Cursors (index));
         if not Leaf.Is_Leaf then
            Feature_Index := Leaf.Feature_Index;
            Put_Line ("Tree.Predict Feature_Index" &
                        Integer'Image (Feature_Index));
            Features_List := X.Element (index);
            Target.Append (Features_List (Feature_Index));
         end if;
      end loop;

      return Target;

   end Predict;

   --  -------------------------------------------------------------------------
   --     function Validate_X_Predict (Self         : Validation.Attribute_List;
   --                                   X           : Sample_Matrix;
   --                                   Check_Input : Boolean := True)
   --                                  return Sample_Matrix is
   --        Self_Length  : constant Integer := Integer (Self.Length);
   --        N_Features : constant Integer := X'Length (2);
   --     begin
   --        if N_Features /= Self_Length then
   --           raise Value_Error with
   --             "Number of features of the model must match the input." &
   --             " Model n_features is " & Integer'Image (Self_Length) &
   --             "and input n_features is " & Integer'Image (N_Features);
   --        end if;
   --        return X;
   --     end Validate_X_Predict;

   --  -------------------------------------------------------------------------

end Tree;
