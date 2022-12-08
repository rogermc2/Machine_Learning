
with Ada.Containers;
with Ada.Text_IO; use Ada.Text_IO;

package body Tree_Printing is

   procedure Print_Value_Record (Name : String; Value : ML_Types.Value_Record);

   --  -------------------------------------------------------------------------

   procedure Print_Boolean_Matrix (Name : String; aMatrix : Boolean_Matrix) is
   begin
      Put_Line (Name & ": ");
      for row in aMatrix'First .. aMatrix'Last loop
         for col in aMatrix'First (2)  .. aMatrix'Last (2) loop
            Put (Boolean'Image (aMatrix (row, col)) & "  ");
         end loop;
         New_Line;
      end loop;
      New_Line;

   end Print_Boolean_Matrix;

   --  ------------------------------------------------------------------------

   procedure Print_Boolean_List
     (Name   : String; aList : NL_Types.Boolean_List;
      Start  : Integer := 1; Finish : Integer := 0) is
      Last  : Integer;
      Count : Integer := 1;
   begin
      if Finish > 0 then
         Last := Finish;
      else
         Last := Integer (aList.Length);
      end if;

      Put_Line (Name & ": ");
      if Start >= aList.First_Index and then Finish <= aList.Last_Index then
         for Index in Start .. Last loop
            Put (Boolean'Image (aList.Element (Index)) & "   ");
            Count := Count + 1;
            if Count > 10 then
               New_Line;
               Count := 1;
            end if;
         end loop;

      else
         Put_Line
           ("Print_Boolean_List called with invalid start or finish index.");
      end if;
      New_Line;

   end Print_Boolean_List;

   --  ------------------------------------------------------------------------

   procedure Print_Criterion (Name : String;
                              Data : Criterion.Criterion_Class) is
      use Criterion;
      use NL_Types.Natural_List_Package;
      use NL_Types.Float_Package;
   begin
      Put_Line (Name & ": ");
      Put_Line ("Criterion_Type: " &
                  Criterion_Kind'Image (Data.Criterion_Type));
      Put_Line ("Y length:              " &
                  Integer'Image (Integer (Data.Y.Length)));
      Put_Line ("Num_Outputs:           " &
                  Integer'Image (Integer (Data.Num_Outputs)));
      Put_Line ("Sample_Weight length:  " &
                  Integer'Image (Integer (Data.Sample_Weight.Length)));
      Put_Line ("Samples length:        " &
                  Integer'Image (Integer (Data.Samples.Length)));
      Put_Line ("Sample_Indices length: " &
                  Integer'Image (Integer (Data.Sample_Indices.Length)));
      Put_Line ("Start_Row: " & Natural'Image (Data.Start_Row));
      Put_Line ("Stop_Row:  " & Natural'Image (Data.Stop_Row));
      Put_Line ("Split_Row: " & Natural'Image (Data.Split_Row));
      Put_Line ("Num_Node_Samples:          " &
                  Natural'Image (Data.Num_Node_Samples));
      Put_Line ("Num_Weighted_Samples:      " &
                  Float'Image (Data.Num_Weighted_Samples));
      Put_Line ("Num_Weighted_Node_Samples: " &
                  Float'Image (Data.Num_Weighted_Node_Samples));
      Put_Line ("Num_Weighted_Left:         " &
                  Float'Image (Data.Num_Weighted_Left));
      Put_Line ("Num_Weighted_Right:        " &
                  Float'Image (Data.Num_Weighted_Right));
      Print_Weight_Lists_2D ("Sum_Total", Data.Sum_Total);
      Print_Weight_Lists_2D ("Sum_Left", Data.Sum_Left);
      Print_Weight_Lists_2D ("Sum_Right", Data.Sum_Right);
      Put_Line ("Proxy_Improvement: " & Float'Image (Data.Proxy_Improvement));
      case Data.Criterion_Type is
         when Criterion_Classification =>
            Print_Natural_List ("Num_Classes", Data.Num_Classes);
         when Criterion_Regression =>
            Put_Line ("Sq_Sum_Total: " & Float'Image (Data.Sq_Sum_Total));
      end case;
      New_Line;

   end Print_Criterion;

   --  ------------------------------------------------------------------------

   --      generic
   --          type Index_Type is (<>);
   --          type Vector_Type is  array (Index_Type) of aliased Float;
   --      procedure Print_Floats_Vector (Name : String; aVector : Vector_Type);
   --
   --      procedure Print_Floats_Vector (Name : String; aVector : Vector_Type) is
   --      begin
   --          if Name = "" then
   --              Put ("  ");
   --          else
   --              Put (Name & ":  ");
   --          end if;
   --          for Index in aVector'Range loop
   --              Put (Float'Image (aVector (Index)) & "   ");
   --          end loop;
   --          New_Line;
   --      end Print_Floats_Vector;

   --  -------------------------------------------------------------------

   --      generic
   --          type Index_Type is (<>);
   --          type Vector_Type is  array (Index_Type) of aliased Integer;
   --      procedure Print_Integer_Vector (Name : String; aVector : Vector_Type);
   --
   --      --  -------------------------------------------------------------------
   --
   --      procedure Print_Integer_Vector (Name : String; aVector : Vector_Type) is
   --      begin
   --          if Name = "" then
   --              Put ("  ");
   --          else
   --              Put (Name & ":  ");
   --          end if;
   --          for Index in aVector'Range loop
   --              Put (Integer'Image (aVector (Index)) & "   ");
   --          end loop;
   --          New_Line;
   --      end Print_Integer_Vector;

   --  -------------------------------------------------------------------

   procedure Print_Float_Array (Name  : String; anArray : Float_Array;
                                Start : Integer := 1; Finish : Integer := 0) is
      Last  : Integer;
      Count : Integer := 1;
   begin
      if Finish > 0 then
         Last := Finish;
      else
         Last := Integer (anArray'Length);
      end if;

      Put_Line (Name & ": ");
      if Start >= anArray'First and then Finish <= anArray'Last then
         for Index in Start .. Last loop
            Put (Float'Image (anArray (Index)) & "  ");
            Count := Count + 1;
            if Count > 4 then
               New_Line;
               Count := 1;
            end if;
         end loop;
      else
         Put_Line
           ("Print_Float_Array called with invalid start or finish index.");
      end if;
      New_Line;

   end Print_Float_Array;

   --  ------------------------------------------------------------------------

   procedure Print_Float_List (Name : String; theList : NL_Types.Float_List) is
      Count : Integer := 1;
   begin
      Put_Line (Name & ": ");
      for Index in theList.First_Index .. theList.Last_Index loop
         Put (Integer'Image (Index) & ": " &
                Float'Image (theList.Element (Index)) & "   ");
         Count := Count + 1;
         if Count > 4 then
            New_Line;
            Count := 1;
         end if;
      end loop;
      New_Line;
   end Print_Float_List;

   --  ------------------------------------------------------------------------

   procedure Print_Integer_Array (Name : String; anArray : Integer_Array) is
   begin
      Put_Line (Name & ": ");
      for Index in anArray'First .. anArray'Last loop
         Put_Line (Integer'Image (Index) & ":  " &
                     Integer'Image (anArray (Index)));
      end loop;
      New_Line;
   end Print_Integer_Array;

   --  ------------------------------------------------------------------------

   procedure Print_Multi_Value_Array (Name    : String;
                                      anArray : Multi_Value_Array) is
   begin
      Put (Name);
      if anArray'Length > 0 and anArray'First > 0 then
         Put_Line (": ");
         for Index in anArray'First .. anArray'Last loop
            Put_Line (Integer'Image (anArray (Index, 1)) & ",  " &
                        Integer'Image (anArray (Index, 2)));
         end loop;
      elsif anArray'Length = 0 then
         Put_Line (" is empty.");
      else
         raise Print_Error with
           "Print_Multi_Value_Array called with invalid index: " &
           Integer'Image (Integer (anArray'First));
      end if;

      New_Line;
   end Print_Multi_Value_Array;

   --  ------------------------------------------------------------------------

   procedure Print_Integer_List (Name : String; theList : ML_Types.Integer_List) is
      Count : Integer := 1;
   begin
      Put_Line (Name & ": ");
      for Index in theList.First_Index .. theList.Last_Index loop
         Put (Integer'Image (theList.Element (Index)) & "   ");
         Count := Count + 1;
         if Count > 10 then
            New_Line;
            Count := 1;
         end if;
      end loop;
      New_Line;

   end Print_Integer_List;

   --  ------------------------------------------------------------------------

   procedure Print_Float_Lists_2D (Name : String;
                                   Data : NL_Types.Float_List_2D) is
   begin
      Put_Line (Name & ": ");
      for index in Data.First_Index .. Data.Last_Index loop
         Print_Float_List ("List" & Integer'Image (index),
                           Data.Element (index));
      end loop;
      New_Line;

   end Print_Float_Lists_2D;

   --  ------------------------------------------------------------------------

   procedure Print_Natural_Lists_2D (Name : String;
                                     Data : NL_Types.Natural_Lists_2D) is
   begin
      Put_Line (Name & ": ");
      for Index in Data.First_Index .. Data.Last_Index loop
         Print_Natural_List ("", Data.Element (Index));
      end loop;
      New_Line;

   end Print_Natural_Lists_2D;

   --  ------------------------------------------------------------------------

   procedure Print_Value_Lists_2D
     (Name : String; Multi_List : Tree.Values_List_2D) is
   begin
      Print_Float_Lists_2D (Name, Multi_List);
   end Print_Value_Lists_2D;

   --  ------------------------------------------------------------------------

   procedure Print_Value_Lists_3D (Name    : String;
                                   theList : Tree.Values_List_3D) is
   begin
      Put_Line (Name & ": ");
      for index in theList.First_Index .. theList.Last_Index loop
         Print_Value_Lists_2D ("List" & Integer'Image (index),
                               theList.Element (index));
      end loop;
      New_Line;
   end Print_Value_Lists_3D;

   --  ------------------------------------------------------------------------

   procedure Print_Natural_List (Name : String;
                                 theList : NL_Types.Natural_List) is
      Count : Integer := 1;
   begin
      if Name'Length > 0 then
         Put_Line (Name & ": ");
      end if;

      for Index in theList.First_Index .. theList.Last_Index loop
         Put (Natural'Image (theList.Element (Index)) & "   ");
         Count := Count + 1;
         if Count > 10 then
            New_Line;
            Count := 1;
         end if;
      end loop;
      New_Line;
   end Print_Natural_List;

   --  ------------------------------------------------------------------------

   procedure Print_Node (Node : Tree.Tree_Node; Offset : String := "") is
      UB_Offset   : constant Unbounded_String :=
                      To_Unbounded_String (Offset);
   begin
      Put_Line ("Node" & Integer'Image (Node.Node_ID));
      Put_Line (To_String (UB_Offset) & "Number of samples" &
                  Integer'Image (Node.Num_Node_Samples));
      Put (To_String (UB_Offset & "Type of node: "));

      if Node.Leaf_Node then
         Put_Line ("Leaf");
      else
         Put_Line ("Decision");
      end if;

      Put_Line (To_String (UB_Offset & "Number of weighted samples:" &
                  Integer'Image (Node.Weighted_Num_Node_Samples)));
      Put_Line (To_String (UB_Offset & "Impurity: " &
                  Float'Image (Node.Impurity)));

      if not Node.Leaf_Node then
         Put_Line (To_String (UB_Offset & "Feature index: " &
                     Integer'Image (Node.Best_Fit_Feature_Index)));
         Put_Line (To_String (UB_Offset & "Threshold: " &
                     Float'Image (Node.Threshold)));
      end if;

   end Print_Node;

   --  ------------------------------------------------------------------------

   procedure Print_Node (Message : String; Node : Tree.Tree_Node) is
   begin
      if Message'Length > 0 then
         Put_Line (Message);
      end if;
      Print_Node (Node);
      New_Line;

   end Print_Node;

   --  ------------------------------------------------------------------------

   procedure Print_Node (Message : String; Node : Tree.Tree_Cursor) is
      use Tree.Nodes_Package;
   begin
      if Message'Length > 0 then
         Put_Line (Message);
      end if;
      Print_Node (Element (Node));
      New_Line;

   end Print_Node;

   --  ------------------------------------------------------------------------

   procedure Print_Node_Cursor_Array (Name    : String;
                                      Cursors : Tree.Leaf_Cursor_Array) is
      use Tree.Nodes_Package;
   begin
      Put_Line (Name & ": ");
      for Index in Cursors'First .. Cursors'Last loop
         Print_Node ("", Element (Cursors (index)));
      end loop;

   end Print_Node_Cursor_Array;

   --  ------------------------------------------------------------------------

   procedure Print_Node_Cursor_List (Name    : String;
                                     Cursors : Tree.Tree_Cursor_List) is
      use Tree.Nodes_Package;
   begin
      Put_Line (Name & ": ");
      for Index in Cursors.First_Index .. Cursors.Last_Index loop
         Print_Node ("", Element (Cursors.Element (index)));
      end loop;

   end Print_Node_Cursor_List;

   --  ------------------------------------------------------------------------

   procedure Print_Split_Record (Name : String;
                                 Data : Node_Splitter.Split_Record) is
   begin
      Put_Line (Name & ": ");
      Put_Line ("Feature_Index:  " & Integer'image (Data.Feature));
      Put_Line ("Split Row:      " & Integer'image (Data.Split_Row));
      Put_Line ("Threshold:      " & Float'image (Data.Threshold));
      Put_Line ("Improvement:    " & Float'image (Data.Improvement));
      Put_Line ("Impurity Left:  " & Float'image (Data.Impurity_Left));
      Put_Line ("Impurity Right: " & Float'image (Data.Impurity_Right));
      New_Line;

   end Print_Split_Record;

   --  ------------------------------------------------------------------------

   procedure Print_Stack_Record (Name : String;
                                 Data : Build_Utils.Stack_Record) is
      use Tree.Nodes_Package;
   begin
      Put_Line (Name & ": ");
      Put_Line ("Parent Node ID: " &
                  Integer'image (Element (Data.Parent_Cursor).Node_ID));
      Put_Line ("Start Row:      " & Integer'Image (Data.Start));
      Put_Line ("Stop Row:       " & Integer'Image (Data.Stop));
      Put_Line ("Depth:          " & Integer'Image (Data.Depth));
      Put_Line ("Branch:         " & Tree.Node_Type'Image (Data.Branch));
      Put_Line ("Impurity:       " & Float'Image (Data.Impurity));
      Put_Line ("Num Constant Features: " &
                  Integer'Image (Data.Num_Constant_Features));
      New_Line;

   end Print_Stack_Record;

   --  ------------------------------------------------------------------------

   procedure Print_Tree (Name  : String;
                         aTree : Base_Decision_Tree.Classifier) is
      Tree_Nodes  : constant Tree.Tree_Class :=
                      aTree.Attributes.Decision_Tree;
   begin
      Print_Tree (Name, Tree_Nodes);
   end Print_Tree;

   --  -------------------------------------------------------------------------

   procedure Print_Tree (Name  : String;
                         aTree : Tree.Tree_Class) is
      use Tree;
      use Nodes_Package;
      Nodes       : constant Tree_Nodes := aTree.Nodes;
      This_Indent : Natural := 0;
      --  Print_Tree_Node is recursive
      procedure Print_Tree_Node (Curs   : Nodes_Package.Cursor;
                                 Indent : Natural := 0) is
         use Ada.Containers;
         Node   : constant Tree_Node := Element (Curs);
      begin
         This_Indent := Indent + 1;
         if This_Indent > 10 then
            This_Indent := 1;
         end if;

         declare
            Offset : String (1 .. This_Indent + 1) := (others => ' ');
            pos    : Natural := 1;
         begin
            while pos < This_Indent - 1 loop
               Offset (pos .. pos + 2) := "   ";
               pos := pos + 2;
            end loop;

            if This_Indent > 1 and then pos < This_Indent + 1 then
               Offset (Indent) := ' ';
            end if;

            Print_Node (Node, Offset);

            if not Is_Leaf (Curs) then
               Print_Tree_Node (First_Child (Curs));
               if Child_Count (Curs) > 1 then
                  Print_Tree_Node (Next_Sibling (First_Child (Curs)));
               end if;
            end if;
         end; --  declare block

      end Print_Tree_Node;

   begin
      Put_Line (Name);
      Print_Tree_Node (First_Child (Nodes.Root));

   end Print_Tree;

   --  -------------------------------------------------------------------------

   procedure Print_Unbounded_List (Name    : String;
                                   theList : ML_Types.Unbounded_List) is
      Count : Integer := 1;
   begin
      Put_Line (Name & ": ");
      for Index in theList.First_Index .. theList.Last_Index loop
         Put (Integer'Image (Index) & ": " &
                To_String (theList.Element (Index)) & "   ");
         Count := Count + 1;
         if Count > 4 then
            New_Line;
            Count := 1;
         end if;
      end loop;
      New_Line;
   end Print_Unbounded_List;

   --  ------------------------------------------------------------------------

   procedure Print_Unbounded_Set (Name   : String;
                                  theSet : Encode_Utils.UB_String_Sets.Set) is
      use Encode_Utils;
      UB_Strings_Curs : UB_String_Sets.Cursor := theSet.First;
      Count           : Integer := 1;
   begin
      if Name'Length > 0 then
         Put (Name & ": ");
      end if;

      while UB_String_Sets.Has_Element (UB_Strings_Curs) loop
         Put (To_String (UB_String_Sets.Element (UB_Strings_Curs)) & "   ");
         Count := Count + 1;
         if Count > 10 then
            New_Line;
            Count := 1;
         end if;
         UB_String_Sets.Next (UB_Strings_Curs);
      end loop;
      New_Line;

   end Print_Unbounded_Set;

   --  ------------------------------------------------------------------------

   procedure Print_Value_List (Name    : String;
                               theList : Tree.Values_List) is
      Count : Integer := 1;
   begin
      Put_Line (Name & ": ");
      for Index in theList.First_Index .. theList.Last_Index loop
         Put ("   " & Float'Image (theList.Element (Index)));
         Count := Count + 1;
         if Count > 5 then
            New_Line;
            Count := 1;
         end if;
      end loop;
      New_Line;
   end Print_Value_List;

   --  ------------------------------------------------------------------------

   procedure Print_Value_Data_List (Name    : String;
                                    theList : ML_Types.Value_Data_List) is
      Value : ML_Types.Value_Record;
      Count : Integer := 1;
   begin
      if Name'Length > 0 then
         Put_Line (Name);
      end if;

      Put_Line ("List length: " & Integer'Image (Integer (theList.Length)));
      for Index in theList.First_Index .. theList.Last_Index loop
         Value := theList.Element (Index);
         Put ("   ");
         Print_Value_Record ("", Value);
         Count := Count + 1;
         if Count > 10 then
            New_Line;
            Count := 1;
         end if;
      end loop;
      New_Line;

   end Print_Value_Data_List;

   --  ------------------------------------------------------------------------

   procedure Print_Value_Record (Name : String; Value : ML_Types.Value_Record) is
      use ML_Types;
   begin
      if Name'Length > 0 then
         Put_Line (Name & ":");
      end if;

      case Value.Value_Kind is
         when Boolean_Type => Put (Boolean'Image (Value.Boolean_Value));
         when Float_Type => Put (Float'Image (Value.Float_Value));
         when Integer_Type => Put (Integer'Image (Value.Integer_Value));
         when UB_String_Type => Put (To_String (Value.UB_String_Value));
      end case;

      if Name'Length > 0 then
         New_Line;
      end if;

   end Print_Value_Record;

   --  ------------------------------------------------------------------------

   procedure Print_Weights (Name : String; Data : Weights.Weight_List) is
      aWeight : Float;
      Count   : Integer := 1;
   begin
      if Name'Length > 0 then
         Put_Line (Name & ": ");
      end if;

      for Index in Data.First_Index .. Data.Last_Index loop
         aWeight := Data.Element (Index);
         Put (Float'Image (aWeight) & "   ");
         Count := Count + 1;
         if Count > 10 then
            New_Line;
            Count := 1;
         end if;
      end loop;
      New_Line;
   end Print_Weights;

   --  ------------------------------------------------------------------------

   procedure Print_Weight_Lists_2D (Name : String;
                                    Data : Weights.Weight_Lists_2D) is
   begin
      Put_Line (Name & ": ");
      for Index in Data.First_Index .. Data.Last_Index loop
         Print_Weights ("", Data.Element (Index));
      end loop;
      New_Line;

   end Print_Weight_Lists_2D;

   --  ------------------------------------------------------------------------

   procedure Print_Weight_Lists_3D (Name : String;
                                    Data : Weights.Weight_Lists_3D) is
   begin
      Put_Line (Name & ": ");
      for Index in Data.First_Index .. Data.Last_Index loop
         Print_Weight_Lists_2D ("Output list" & Integer'Image (index),
                                Data.Element (Index));
      end loop;
      New_Line;

   end Print_Weight_Lists_3D;

   --  ------------------------------------------------------------------------

end Tree_Printing;
