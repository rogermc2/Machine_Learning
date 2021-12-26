--  Based on scikit-learn/sklearn/tree _tree.pyx class Tree

with Ada.Assertions; use Ada.Assertions;
with Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;

with Classifier_Types;
with Printing;

package body Tree is

   function Apply_Dense (Self : Tree_Class; X : ML_Types.Value_Data_Lists_2D)
                         return Classifier_Types.Natural_List;
   function Decision_Path_Dense (aTree : Tree_Class;
                                 X     : ML_Types.Value_Data_Lists_2D)
                                 return Classifier_Types.Natural_Lists_2D;

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
   --  L780
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
      use Ada.Strings.Unbounded;
      use ML_Types;
      use Value_Data_Package;
      use Nodes_Package;
      Routine_Name   : constant String := "Tree.Apply_Dense ";
      Top_Cursor     : constant Tree_Cursor := First_Child (Self.Nodes.Root);
      Num_Samples    : constant Positive := Positive (X.Length);
      Node_Cursor    : Tree_Cursor;
      Node           : Tree_Node;
      Current_Sample : Value_Data_List;
      Feature_Value  : Value_Record;
      Out_Data       : Classifier_Types.Natural_List;
      Continue       : Boolean;
      Use_Left       : Boolean;
   begin
      Assert (Integer (Child_Count (Top_Cursor)) > 0, Routine_Name &
                "Top node has no children");
      --  L796
      for index in 1 .. Num_Samples loop
         Out_Data.Append (0);
      end loop;

      --  L804 for each sample
      for index in X.First_Index .. X.Last_Index loop
         Node_Cursor := Top_Cursor;
         --  Current_Sample is a list of feature values
         Current_Sample := X.Element (index);

         --  Find a node with a leaf child.
         --  This node has the prediction value.
         Continue := True;
         while Continue and then
           Child_Count (Node_Cursor) > 0 loop
            Node := Element (Node_Cursor);
            Feature_Value :=
              Current_Sample.Element (Node.Best_Fit_Feature_Index);
            --                 Printing.Print_Value_Record (Routine_Name & "Feature_Value",
            --                                              Feature_Value);
            Assert (Feature_Value.Value_Kind = Float_Type or
                      Feature_Value.Value_Kind = Integer_Type,
                    "Tree.Apply_Dense Self.Nodes invalid feature data type");
            --  Make tree traversal decision
            case Feature_Value.Value_Kind is
               when Boolean_Type =>
                  if True then
                     Use_Left := 1.0 <= Node.Threshold;
                  else
                     Use_Left := 0.0 <= Node.Threshold;
                  end if;
               when Float_Type =>
                  Use_Left := Feature_Value.Float_Value <= Node.Threshold;
               when Integer_Type =>
                  Use_Left := Float (Feature_Value.Integer_Value) <=
                    Node.Threshold;
               when UB_String_Type =>
                  declare
                     Comp : Float := 0.0;
                     Text : constant String :=
                              To_String (Feature_Value.UB_String_Value);
                  begin
                     for pos in 1 .. Text'Length loop
                        Comp := Comp +
                          Float (Integer'Value (Text (pos .. pos)));
                     end loop;
                     Use_Left := Comp <= Node.Threshold;
                  end;
            end case;

            if Use_Left then
               --                    Put_Line (Routine_Name & "use left child Node_ID" &
               --                                Integer'Image
               --                                (Element (First_Child (Node_Cursor)).Node_ID));
               Node_Cursor := First_Child (Node_Cursor);
            else
               --                    Put_Line (Routine_Name & "use right child Node_ID" &
               --                                Integer'Image
               --                                (Element (Last_Child (Node_Cursor)).Node_ID));
               Node_Cursor := Last_Child (Node_Cursor);
            end if;
            Continue := Child_Count (Node_Cursor) > 0;

         end loop;  --  Not_Leaf
         Out_Data.Replace_Element (index, Element (Node_Cursor).Node_ID);
      end loop;
      --        Printing.Print_Natural_List (Routine_Name & "Out_Data", Out_Data);
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

   function Decision_Path
     (aTree : Tree_Class; X : ML_Types.Value_Data_Lists_2D)
      return Classifier_Types.Natural_Lists_2D is
   begin
      return Decision_Path_Dense (aTree, X);

   end Decision_Path;

   --  -------------------------------------------------------------------------

   function Decision_Path_Dense
     (aTree : Tree_Class; X : ML_Types.Value_Data_Lists_2D)
      return Classifier_Types.Natural_Lists_2D is
      use Ada.Containers;
      use Ada.Strings.Unbounded;
      use ML_Types;
      use Value_Data_Package;
      use Nodes_Package;
      Routine_Name   : constant String := "Tree.Decision_Path_Dense ";
      Top_Cursor     : constant Tree_Cursor := First_Child (aTree.Nodes.Root);
      Num_Samples    : constant Positive := Positive (X.Length);
      Node_Cursor    : Tree_Cursor;
      Node           : Tree_Node;
      Node_ID_List   : Classifier_Types.Natural_List;
      Current_Sample : Value_Data_List;
      Feature_Value  : Value_Record;
      Use_Left       : Boolean;
      Continue       : Boolean := True;
      --  Out_Data: num samples x num nodes
      Out_Data       : Classifier_Types.Natural_Lists_2D;
   begin
     Assert (Integer (Child_Count (Top_Cursor)) > 0, Routine_Name &
                "Top node has no children");
         Put_Line (Routine_Name & "Num_Samples" & Integer'Image (Num_Samples));
         Put_Line (Routine_Name & "Max_Depth" & Integer'Image (aTree.Max_Depth));

      --  L924 for each sample
      for index in X.First_Index .. X.Last_Index loop
         New_Line;
         Put_Line (Routine_Name & "Sample " & Integer'Image (index));
         Node_Cursor := Top_Cursor;
         --  Current_Sample is a list of feature values
         Current_Sample := X.Element (index);
         Printing.Print_Value_Data_List (Routine_Name & "Current_Sample",
                                         Current_Sample);

         --  L928 Add all external nodes
         Continue := True;
         Node_ID_List.Clear;

         while Continue and then
           Child_Count (Node_Cursor) > 0 loop
            Node := Element (Node_Cursor);
            Feature_Value :=
              Current_Sample.Element (Node.Best_Fit_Feature_Index);
            Put_Line (Routine_Name & "Node_ID " & Integer'Image (Node.Node_ID));
            Printing.Print_Value_Record (Routine_Name & "Feature_Value",
                                         Feature_Value);
            Node_ID_List.Append (Node.Node_ID);
            Assert (Feature_Value.Value_Kind = Float_Type or
                      Feature_Value.Value_Kind = Integer_Type,
                    "Tree.Apply_Dense Self.Nodes invalid feature data type");
            --  Make tree traversal decision
            case Feature_Value.Value_Kind is
               when Boolean_Type =>
                  if True then
                     Use_Left := 1.0 <= Node.Threshold;
                  else
                     Use_Left := 0.0 <= Node.Threshold;
                  end if;
               when Float_Type =>
                  Use_Left := Feature_Value.Float_Value <= Node.Threshold;
               when Integer_Type =>
                  Use_Left := Float (Feature_Value.Integer_Value) <=
                    Node.Threshold;
               when UB_String_Type =>
                  declare
                     Comp : Float := 0.0;
                     Text : constant String :=
                              To_String (Feature_Value.UB_String_Value);
                  begin
                     for pos in 1 .. Text'Length loop
                        Comp := Comp +
                          Float (Integer'Value (Text (pos .. pos)));
                     end loop;
                     Use_Left := Comp <= Node.Threshold;
                  end;
            end case;

            if Use_Left then
               Node_Cursor := First_Child (Node_Cursor);
            else
               Node_Cursor := Last_Child (Node_Cursor);
            end if;

            Continue := Child_Count (Node_Cursor) > 0;
         end loop;  --  Not_Leaf

         --  L939 Add leaf node
         Node := Element (Node_Cursor);
         Node_ID_List.Append (Node.Node_ID);
         Out_Data.Append (Node_ID_List);
      end loop;

      return Out_Data;

   end Decision_Path_Dense;

   --  -------------------------------------------------------------------------
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
      for index in Samples.First_Index .. Samples.Last_Index loop
         if Samples.Element (index) > 0 then
            Out_Data.Append (Values.Element (Samples.Element (index)));
         end if;
      end loop;

      return Out_Data;

   end Predict;

   --  -------------------------------------------------------------------------

end Tree;
