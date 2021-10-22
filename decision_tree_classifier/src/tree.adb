--  Based on scikit-learn/sklearn/tree _tree.pyx class Tree

with Ada.Assertions; use Ada.Assertions;
--  with Ada.Text_IO; use Ada.Text_IO;

with Classifier_Utilities;

package body Tree is

   --  -------------------------------------------------------------------------
   --  L770 Apply finds the terminal region (=leaf node) for each sample in X.
--     function Apply (Self : Tree_Class;
--                     X    : ML_Types.List_Of_Value_Data_Lists)
--                     return Leaf_Cursor_Array is
--     begin
--        return Apply_Dense (Self, X);
--     end Apply;

   --  -------------------------------------------------------------------------
   --  L777
--     function Apply_Dense (Self : Tree_Class;
--                           X    : ML_Types.List_Of_Value_Data_Lists)
--                           return Leaf_Cursor_Array is
--        --  X is a list of samples of features
--        use Ada.Containers;
--        use ML_Types;
--        use Nodes_Package;
--        Num_Samples    : constant Positive := Positive (X.Length);
--        Top_Cursor     : constant Tree_Cursor := First_Child (Self.Nodes.Root);
--        Node_Cursor    : Tree_Cursor;
--        Left_Cursor    : Tree_Cursor;
--        Node           : Tree_Node;
--        Left_Node      : Tree_Node;
--        Sample         : Value_Data_List;
--        Feature_Value  : Value_Record;
--        Not_Leaf       : Boolean := True;
--        Split          : Boolean;
--        Output_Cursors : Leaf_Cursor_Array (1 .. Num_Samples);
--     begin
--        Assert (Integer (Child_Count (Top_Cursor)) > 0,
--                "Tree.Apply_Dense Self.Nodes tree is empty");
--
--        --  for each sample of features
--        for index in X.First_Index .. X.Last_Index loop
--           Sample := X.Element (index);
--           Node_Cursor := Top_Cursor;
--           Not_Leaf := True;
--           Put_Line ("Tree.Apply_Dense sample index: " &
--                       Integer'Image (index));
--           while Not_Leaf loop
--              Node := Element (Node_Cursor);
--              Left_Cursor := First_Child (Node_Cursor);
--              Not_Leaf := not Element (Left_Cursor).Leaf_Node;
--              Put_Line ("Tree.Apply_Dense Left_Cursor Not_Leaf: " &
--                          Boolean'Image (Not_Leaf));
--              if Not_Leaf then
--                 Left_Node := Element (Left_Cursor);
--                 if not Left_Node.Leaf_Node then
--                    Assert (Feature_Value.Value_Kind = Float_Type or
--                              Feature_Value.Value_Kind = Integer_Type,
--                            "Tree.Apply_Dense Self.Nodes invalid feature data type");
--                    Put_Line ("Tree.Apply_Dense Feature_Index: " &
--                                Integer'Image (Node.Feature_Index));
--                    Feature_Value := Sample.Element (Node.Feature_Index);
--                    case Feature_Value.Value_Kind is
--                    when Float_Type =>
--                       Split := Feature_Value.Float_Value <= Node.Threshold;
--                    when Integer_Type =>
--                       Split := Float (Feature_Value.Integer_Value) <=
--                         Node.Threshold;
--                    when others => null;
--                    end case;
--
--                    Put_Line ("Tree.Apply_Dense Node_Cursor Split: " &
--                                Boolean'Image (Split));
--                    if Split then
--                       Node_Cursor := First_Child (Node_Cursor);
--                    else
--                       Node_Cursor := Last_Child (Node_Cursor);
--                    end if;
--                 end if;
--
--              end if;  --  Not_Leaf
--           end loop;  --  Not_Leaf
--
--           Output_Cursors (index) := Node_Cursor;
--           New_Line;
--        end loop;
--
--        return Output_Cursors;
--
--     end Apply_Dense;

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
                     return Tree.Values_Array_3D is
      --  X is a list of samples
      --  Each sample is a list of feature values, one value per feature
--        N_Samples       : constant Natural := Natural (X.Length);
      Num_Outputs     : constant Positive := Positive (X.Element (1).Length);
      Num_Nodes       : constant Positive := Positive (Self.Nodes.Node_Count);
      Num_Classes     : constant Positive := Positive (Self.Classes.Length);
      --  Apply finds the terminal region (=leaf node) for each sample in X.
      --  Leaf_Cursors is a list of feature cursors, each cursor corresponding to
      --  a leaf noode of X
--        Leaf_Cursors    : Leaf_Cursor_Array (1 .. N_Samples);
--        Leaf            : Tree_Node;
--        Feature_Index   : Positive;
      Node_Index      : Natural := 0;
--        Outputs_List    : List_Of_Value_Data_Lists;
--        Features_List   : Value_Data_List;
      Out_Values      : Values_Array_3D (1 .. Num_Nodes, 1 .. Num_Outputs,
                                         1 .. Num_Classes);
--        Output          : Value_Data_List;
      --  L1086 _get_value_ndarray
      --  Output should be a 3D array (node count, num outputs, max classes)
      --  self.value
      --  Leaf_Cursors should provide node-count nodes but
      --  provides num-samples nodes
      --  (Self.Nodes.Node_Count, Self.Num_Outputs, Self.Classes.Length)
      procedure Build_Output (Curs : Cursor) is
         --  Tree_Node.Values is num_outputs x num_classes
         Node    : constant Tree_Node := Element (Curs);
         Values  : constant List_Of_Values_Lists :=
                    Node.Values;
         Outputs : Values_List;
      begin
         Node_Index := Node_Index + 1;
         for output_index in 1 .. Num_Outputs loop
            Outputs := Values.Element (output_index);
            for class_index in 1 .. Num_Classes loop
               Out_Values (Node_Index, output_index, class_index) :=
                 Outputs.Element (class_index);
            end loop;
         end loop;
      end Build_Output;

   begin
      Assert (Integer (Child_Count (Self.Nodes.Root)) > 0,
              "Tree.Predict Self.Nodes tree is empty");

      Classifier_Utilities.Print_List_Of_Value_Data_Lists
        ("Tree.Predict Self.Nodes X", X);

--        Leaf_Cursors := Apply (Self, X);
      --  L767
      Iterate_Subtree (Self.Nodes.Root, Build_Output'access);

      --  L801
--        for index in 1 .. N_Samples loop
--           Put_Line ("Tree.Predict Self.Nodes index" & Integer'Image (index));
--           Leaf := Element (Leaf_Cursors (index));
--           if Leaf.Leaf_Node then
--              Put_Line ("Tree.Predict Self.Nodes Leaf.Feature_Index" &
--                          Integer'Image (index));
--              Feature_Index := Leaf.Feature_Index;
--              Features_List := X.Element (index);
--              Output.Append (Features_List (Feature_Index));
--           end if;
--        end loop;

--        Classifier_Utilities.Print_Value_Data_List ("Tree.Predict Output", Output);
      return Out_Values;

   end Predict;

   --  -------------------------------------------------------------------------

end Tree;
