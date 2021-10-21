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
      Node_Cursor    : Tree_Cursor;
      Left_Cursor    : Tree_Cursor;
      Node           : Tree_Node;
      Left_Node      : Tree_Node;
      Sample         : Value_Data_List;
      Feature        : Value_Record;
      Not_Leaf       : Boolean := True;
      Split          : Boolean;
      Output_Cursors : Leaf_Cursor_Array (1 .. Num_Samples);
   begin
      Assert (Integer (Child_Count (Self.Nodes.Root)) > 0,
              "Tree.Apply_Dense Self.Nodes tree is empty");

      --  for each sample of features
      for index in X.First_Index .. X.Last_Index loop
         Sample := X.Element (index);
         Node_Cursor := First_Child (Self.Nodes.Root);
--           while Not_Leaf and then Has_Element (Node_Cursor) loop
         while Not_Leaf loop
            Node := Element (Node_Cursor);
            Left_Cursor := First_Child (Node_Cursor);
            Not_Leaf := not Element (Left_Cursor).Is_Leaf;
            if Not_Leaf then
               Left_Node := Element (Left_Cursor);
               if not Left_Node.Is_Leaf then
                  Assert (Feature.Value_Kind = Float_Type or
                            Feature.Value_Kind = Integer_Type,
                          "Tree.Apply_Dense Self.Nodes invalid feature data type");
                  Feature := Sample.Element (Node.Feature_Index);
                  case Feature.Value_Kind is
                  when Float_Type =>
                     Split := Feature.Float_Value <= Node.Threshold;
                  when Integer_Type =>
                     Split := Float (Feature.Integer_Value) <= Node.Threshold;
                  when others => null;
                  end case;
                  if Split then
                     Node_Cursor := First_Child (Node_Cursor);
                  else
                     Node_Cursor := Last_Child (Node_Cursor);
                  end if;
               end if;
            end if;
         end loop;

         Output_Cursors (index) := Node_Cursor;
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
      Target          : Value_Data_List;
   begin
      Assert (Integer (Child_Count (Self.Nodes.Root)) > 0,
              "Tree.Predict Self.Nodes tree is empty");

      Classifier_Utilities.Print_List_Of_Value_Data_Lists
        ("Tree.Predict Self.Nodes X", X);

      Leaf_Cursors := Apply (Self, X);
      Put_Line ("Tree.Predict Self.Nodes Leaf_Cursors length" &
                  Integer'Image (Integer (Leaf_Cursors'Length)));

      for index in 1 .. N_Samples loop
         Put_Line ("Tree.Predict Self.Nodes index" & Integer'Image (index));
         Leaf := Element (Leaf_Cursors (index));
         if Leaf.Is_Leaf then
            Put_Line ("Tree.Predict Self.Nodes Leaf.Feature_Index" &
                        Integer'Image (index));
            Feature_Index := Leaf.Feature_Index;
            Features_List := X.Element (index);
            Target.Append (Features_List (Feature_Index));
         end if;
      end loop;
      Classifier_Utilities.Print_Value_Data_List ("Tree.Predict Target", Target);

      return Target;

   end Predict;

   --  -------------------------------------------------------------------------

end Tree;
