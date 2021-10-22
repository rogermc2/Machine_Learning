--  Based on scikit-learn/sklearn/tree _tree.pyx class Tree

with Ada.Assertions; use Ada.Assertions;
with Ada.Text_IO; use Ada.Text_IO;

with Classifier_Utilities;

package body Tree is

   function Apply_Dense (Self : Tree_Class;
                         X    : ML_Types.List_Of_Value_Data_Lists)
                         return Leaf_Cursor_Array;

   --  -------------------------------------------------------------------------
   --  L770 Apply finds the terminal region (=leaf node) for each sample in X.
   function Apply (Self : Tree_Class;
                   X    : ML_Types.List_Of_Value_Data_Lists)
                   return Leaf_Cursor_Array is
   begin
      return Apply_Dense (Self, X);
   end Apply;

   --  -------------------------------------------------------------------------
   --  L777
   function Apply_Dense (Self : Tree_Class;
                         X    : ML_Types.List_Of_Value_Data_Lists)
                         return Leaf_Cursor_Array is
      --  X is a list of samples of features
      use Ada.Containers;
      use ML_Types;
      use Nodes_Package;
      Num_Samples    : constant Positive := Positive (X.Length);
      Top_Cursor     : constant Tree_Cursor := First_Child (Self.Nodes.Root);
      Node_Cursor    : Tree_Cursor;
      Left_Cursor    : Tree_Cursor;
      Node           : Tree_Node;
      Left_Node      : Tree_Node;
      Sample         : Value_Data_List;
      Feature_Value  : Value_Record;
      Not_Leaf       : Boolean := True;
      Split          : Boolean;
      Output_Cursors : Leaf_Cursor_Array (1 .. Num_Samples);
   begin
      Assert (Integer (Child_Count (Top_Cursor)) > 0,
              "Tree.Apply_Dense Self.Nodes tree is empty");

      --  for each sample of features
      for index in X.First_Index .. X.Last_Index loop
         Sample := X.Element (index);
         Node_Cursor := Top_Cursor;
         Not_Leaf := True;
         Put_Line ("Tree.Apply_Dense sample index: " &
                     Integer'Image (index));
         while Not_Leaf loop
            Node := Element (Node_Cursor);
            Left_Cursor := First_Child (Node_Cursor);
            Not_Leaf := not Element (Left_Cursor).Leaf_Node;
            Put_Line ("Tree.Apply_Dense Left_Cursor Not_Leaf: " &
                     Boolean'Image (Not_Leaf));
            if Not_Leaf then
               Left_Node := Element (Left_Cursor);
               if not Left_Node.Leaf_Node then
                  Assert (Feature_Value.Value_Kind = Float_Type or
                            Feature_Value.Value_Kind = Integer_Type,
                          "Tree.Apply_Dense Self.Nodes invalid feature data type");
                  Put_Line ("Tree.Apply_Dense Feature_Index: " &
                              Integer'Image (Node.Feature_Index));
                  Feature_Value := Sample.Element (Node.Feature_Index);
                  case Feature_Value.Value_Kind is
                  when Float_Type =>
                     Split := Feature_Value.Float_Value <= Node.Threshold;
                  when Integer_Type =>
                     Split := Float (Feature_Value.Integer_Value) <=
                       Node.Threshold;
                  when others => null;
                  end case;

                  Put_Line ("Tree.Apply_Dense Node_Cursor Split: " &
                              Boolean'Image (Split));
                  if Split then
                     Node_Cursor := First_Child (Node_Cursor);
                  else
                     Node_Cursor := Last_Child (Node_Cursor);
                  end if;
               end if;

            end if;  --  Not_Leaf
         end loop;  --  Not_Leaf

         Output_Cursors (index) := Node_Cursor;
         New_Line;
      end loop;

      return Output_Cursors;

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
   --  _tree L763
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
      Leaf_Cursors    : Leaf_Cursor_Array (1 .. N_Samples);
      Leaf            : Tree_Node;
      Feature_Index   : Positive;
      Features_List   : Value_Data_List;
      Output          : Value_Data_List;
   begin
      Assert (Integer (Child_Count (Self.Nodes.Root)) > 0,
              "Tree.Predict Self.Nodes tree is empty");

      Classifier_Utilities.Print_List_Of_Value_Data_Lists
        ("Tree.Predict Self.Nodes X", X);

      Leaf_Cursors := Apply (Self, X);
--        Classifier_Utilities.Print_Node_Cursor_Array
--          ("Tree.Predict Self.Nodes Leaf_Cursors", Leaf_Cursors);
      --  L801
      for index in 1 .. N_Samples loop
         Put_Line ("Tree.Predict Self.Nodes index" & Integer'Image (index));
         Leaf := Element (Leaf_Cursors (index));
         if Leaf.Leaf_Node then
            Put_Line ("Tree.Predict Self.Nodes Leaf.Feature_Index" &
                        Integer'Image (index));
            Feature_Index := Leaf.Feature_Index;
            Features_List := X.Element (index);
            Output.Append (Features_List (Feature_Index));
         end if;
      end loop;

      Classifier_Utilities.Print_Value_Data_List ("Tree.Predict Output", Output);
      return Output;

   end Predict;

   --  -------------------------------------------------------------------------

end Tree;
