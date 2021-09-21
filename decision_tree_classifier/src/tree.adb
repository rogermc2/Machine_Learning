--  Based on scikit-learn/sklearn/tree _tree.pyx class Tree

--  with Maths;

package body Tree is

   function Apply_Dense (Self : Tree_Class;
                         X    : ML_Types.List_Of_Value_Data_Lists)
                         return Tree_Cursor_List;

   --  -------------------------------------------------------------------------
   --  Apply finds the terminal region (=leaf node) for each sample in X.
   function Apply (Self : Tree_Class;
                   X    : ML_Types.List_Of_Value_Data_Lists)
                   return Tree_Cursor_List is
   begin
      return Apply_Dense (Self, X);
   end Apply;

   --  -------------------------------------------------------------------------

   function Apply_Dense (Self : Tree_Class;
                         X    : ML_Types.List_Of_Value_Data_Lists)
                         return Tree_Cursor_List is
      use ML_Types;
      use Tree_Package;
      Node_Cursor  : Tree_Cursor;
      Node         : Tree_Node;
      Feature      : Positive;
      Samples      : Value_Data_List;
      Leaf_Cursors : Tree_Cursor_List;
   begin
      for index in X.First_Index .. X.Last_Index loop
         Samples := X.Element (index);
         Node_Cursor := Self.Nodes.Root;
         Node := Element (Node_Cursor);
         Feature := Node.Feature_Index;
         while not Element (First_Child (Node_Cursor)).Is_Leaf loop
            if Samples.Element (Feature).Float_Value <= Node.Threshold then
               Node_Cursor := First_Child (Node_Cursor);
            else
               Node_Cursor := Last_Child (Node_Cursor);
            end if;
         end loop;
         Leaf_Cursors.Append (Node_Cursor);
      end loop;

      return Leaf_Cursors;

   end Apply_Dense;

   --  -------------------------------------------------------------------------

--     function Get_Values (Self : Tree_Class) return Values_List is
--          Values : Values_List;
--     begin
--
--        return Values;
--
--     end Get_Values;

   --  -------------------------------------------------------------------------

   function Predict (Self : Tree_Class;
                     X    : ML_Types.List_Of_Value_Data_Lists)
                     return ML_Types.Value_Data_List is
      --  X is a list of samples
      --  Each sample is a list of feature values, one value per feature
      use ML_Types;
      --  N_Samples       : constant Integer := X'Length;
      --  Apply finds the terminal region (=leaf node) for each sample in X.
      --  Leaf_Cursors is a list of feature cursors, each cursor corresponding to
      --  a leaf noode of X
      Leaf_Cursors : Tree_Cursor_List := Apply (Self, X);
      Leaf_Cursor  : Tree_Cursor := Leaf_Cursors.First_Element;
      Leaf         : Tree_Node := Element (Leaf_Cursor);
      Values       : Values_List := Self.Values;
      Outputs      : Output_List;
      Classes      : Class_List;
      Target       : Value_Data_List;
   begin
        for index in Leaf_Cursors.First_Index .. Leaf_Cursors.Last_Index loop
            Leaf_Cursor := Leaf_Cursors.Element (index);
            Leaf := Element (Leaf_Cursor);
            Classes.Append (Leaf.Feature_Index);
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
