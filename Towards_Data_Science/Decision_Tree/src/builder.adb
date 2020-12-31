
with Ada.Containers;
with Ada.Text_IO; use Ada.Text_IO;

with Support;

--  Steps for making decision tree:
--  1. Get list of rows (dataset) to be taken into consideration for making
--     decision tree (recursively at each node).
--  2. Calculate the uncertanity of the dataset or Gini impurity or how much
--     the data is mixed up etc.
--  3. Generate a list of all questions that need to be asked at that node.
--  4. Partition the rows into True rows and False rows based on each question
--     asked.
--  5. Calculate the information gain based on the Gini impurity and
--     partitioning of the data from step 5.
--  6. Update the highest information gain based on each question asked.
--  7. Update the best question based on the information gain
--     (higher information gain).
--  8. Divide the node according to the best question.
--  9. Repeat again from step 1 until we get pure node (leaf node).

package body Builder is

   --  -----------------------------------------------------------------------
   --  A Leaf node classifies data.
   --  A Leaf node is a dictionary of classes  (features) (e.g., "Apple") and,
   --  for each class, the number of times that the class appears in the rows
   --  from the training data that reach this leaf.
   function Build_Tree (Rows : Rows_Vector) return Tree_Type is
      use Tree_Package;
      theTree      : Tree_Type := Empty_Tree;
      Root_Curs    : Tree_Cursor := Root (theTree);
      Best_Split   : Best_Split_Data;
      True_Branch  : Decision_Node_Type;
      False_Branch : Decision_Node_Type;
      aLeaf        : Decision_Node_Type (Prediction_Kind);

      procedure Recurse (Rows : Rows_Vector;
                         Curs : in out Tree_Cursor) is
         use Support;
         P_Rows       : Partitioned_Rows;
         True_Curs    : Tree_Cursor;
         False_Curs   : Tree_Cursor;
      begin
         Put_Line ("Recursing, Rows length: " &
                     Integer'Image (Integer (Rows.Length)));
         Best_Split := Find_Best_Split (Rows);
         if Best_Split.Best_Gain = 0.0 then
            Put_Line ("Best_Gain = 0.0");
            aLeaf.Predictions := Support.Class_Counts (Rows);
            theTree.Insert_Child (Parent   => Curs,
                                  Before   => No_Element,
                                  New_Item => aLeaf);
         else
            P_Rows := Partition (Rows, Best_Split.Best_Question);
            True_Branch.Question := Best_Split.Best_Question;
            False_Branch.Question := Best_Split.Best_Question;
            True_Branch.True_Rows := P_Rows.True_Rows;
            False_Branch.False_Rows := P_Rows.False_Rows;
            Put_Line ("Insert True_Branch");
            Print_Rows ("True_Branch.True_Rows", True_Branch.True_Rows);
            Print_Rows ("False_Branch.False_Rows", False_Branch.False_Rows);
            theTree.Insert_Child (Parent   => Curs,
                                  Before   => No_Element,
                                  New_Item => True_Branch,
                                  Position => True_Curs);
            Put_Line ("Insert False_Branch");
            theTree.Insert_Child (Parent   => Curs,
                                  Before   => No_Element,
                                  New_Item => False_Branch,
                                  Position => False_Curs);
            Put_Line ("Recurse True_Rows");
            Recurse (P_Rows.True_Rows, True_Curs);
            Put_Line ("Recurse False_Rows");
            Recurse (P_Rows.False_Rows, False_Curs);
            Put_Line ("Recurse loop");
         end if;
         New_Line;
      end Recurse;

   begin
      New_Line;
      Put_Line ("Build Tree");
      Recurse (Rows, Root_Curs);
      Put_Line ("Tree built");
      --        return (Best_Split.Best_Question, True_Branch, False_Branch);
      --       return (Best_Split.Best_Question, True_Branch.True_Rows, False_Branch.False_Rows, aLeaf);
      return theTree;
   end Build_Tree;

   --  ---------------------------------------------------------------------------
   --  The root is a special node that is always present and has neither an
   --  associated element value nor any parent node.
   --  The root node provides a place to add nodes to an otherwise empty tree
   --  and represents the base of the tree.
   --  A cursor designates a particular node within a tree and, by extension,
   --  the element contained in that node, if any.
   --  A cursor keeps designating the same node (and element) as long as the
   --  node is part of the container even if the node is moved within the
   --  container.
   function Classify (aRow : Row_Data; aTree : Tree_Type)
                      return Count_Package.Map is
      use Tree_Package;
      --  The node at the root has no element so hence the call of First_Child.
      First_Cursor     : constant Cursor := First_Child (Root (aTree));
      True_Cursor      : constant Cursor := First_Child (First_Cursor);
      False_Cursor     : constant Cursor := Last_Child (First_Cursor);
      First_Node       : constant Decision_Node_Type := Element (First_Cursor);
      True_Subtree     : Tree_Type;
      False_Subtree    : Tree_Type;
      True_Sub_Cursor  : constant Cursor := Root (True_Subtree);
      False_Sub_Cursor : constant Cursor := Root (False_Subtree);
      Result           : Count_Package.Map;
   begin
      True_Subtree.Copy_Subtree (True_Sub_Cursor, No_Element, True_Cursor);
      False_Subtree.Copy_Subtree (False_Sub_Cursor, No_Element, False_Cursor);
      case First_Node.Node_Type is
         when Prediction_Kind =>
            --  Leaf
            Result := First_Node.Predictions;
         when Decision_Kind =>
            if Match (First_Node.Question, aRow) then
               Result := Classify (aRow, True_Subtree);
            else
               Result := Classify (aRow, False_Subtree);
            end if;
      end case;
      return Result;
   end Classify;

   --  -----------------------------------------------------------------------
   --   Find_Best_Split finds the best question to ask by iterating over every
   --   feature / value and calculating the information gain.
   function Find_Best_Split (Rows : Rows_Vector)
                             return Best_Split_Data is
      use Ada.Containers;
      use Support;

      Rows_Length         : constant Integer := Integer (Rows.Length);
      --        Row_Vector          : constant Rows_Vector := To_Vector (Rows);
      Colour_Values       : array (1 .. Rows_Length) of Colour_Type;
      Dimension_Values    : array (1 .. Rows_Length) of Integer;
      Colour_Question     : Question_Type;
      Dimension_Question  : Question_Type (Diameter_Feature);
      Split_Row           : Partitioned_Rows;
      Best_Gain           : Float := 0.0;
      Best_Question       : Question_Type;
      Current_Uncertainty : constant Float := Gini (Rows);

      procedure Test_Gain is
         Gain             : Float := 0.0;
      begin
         if Split_Row.True_Rows.Length /= 0 and
           Split_Row.False_Rows.Length /= 0 then
            Gain := Information_Gain (Split_Row.True_Rows, Split_Row.False_Rows,
                                      Current_Uncertainty);
            if Gain > Best_Gain then
               Best_Gain := Gain;
               Best_Question := Colour_Question;
            end if;
         end if;
      end Test_Gain;

   begin
      for col in 1 .. Rows_Length loop
         Colour_Values (col) := Rows.Element (col).Colour;
         Dimension_Values (col) := Rows.Element (col).Diameter;
      end loop;

      for col in 1 .. Colour_Values'Length loop
         Put_Line ("Find_Best_Split col: " & Integer'Image (col));
         Colour_Question.Colour_Value := Colour_Values (col);
         Dimension_Question.Diameter_Value := Dimension_Values (col);
         Split_Row := Partition (Rows, Colour_Question);
         Test_Gain;
         Put_Line ("Find_Best_Split Best Gain: " & Float'Image (Best_Gain));
         Split_Row := Partition (Rows, Dimension_Question);
         Test_Gain;
         Put_Line ("Find_Best_Split Best Gain: " & Float'Image (Best_Gain));
      end loop;

      return (Best_Gain, Best_Question);
   end Find_Best_Split;

   --  ---------------------------------------------------------------------------

   function Match (Self : Question_Type; Example : Row_Data) return Boolean is
      C       : constant Feature_Type := Self.Feature;
      Matches : Boolean := False;
   begin
      case C is
         when Colour_Feature =>
            declare
               Value : constant Colour_Type := Example.Colour;
            begin
               Matches := Value = self.Colour_Value;
            end;
         when Diameter_Feature =>
            declare
               Value : constant Integer := Example.Diameter;
            begin
               Matches := Value = self.Diameter_Value;
            end;
      end case;
      return Matches;
   end Match;

   --  ---------------------------------------------------------------------------

   function Partition (Rows : Rows_Vector; Question : Question_Type)
                       return Partitioned_Rows is
      True_Rows  : Rows_Vector;
      False_Rows : Rows_Vector;
      Data       : Row_Data;
   begin
      for index in Rows.First_Index .. Rows.Last_Index loop
         Data := Rows.Element (index);
         if Match (Question, Data) then
            True_Rows.Append (Data);
         else
            False_Rows.Append (Data);
         end if;
      end loop;
      return (True_Rows, False_Rows);
   end Partition;

   --  --------------------------------------------------------------------------

   function Print_Leaf (Counts : Count_Package.Map) return Strings_List is
      use Count_Package;
      Total         : Integer := 0;
      aCount        : Natural;
      aString       : Unbounded_String;
      Probabilities : Strings_List;
   begin
      New_Line;
      Put_Line ("Probabilities:");
      for index in Counts.First_Key .. Counts.Last_Key loop
         aCount := Counts.Element (index);
         Total := Total + aCount;
         aString :=
           To_Unbounded_String (Natural'Image (100 * aCount / Total)) & "%";
         Probabilities.Append (aString);
         Put_Line (To_String (aString));
      end loop;
      return Probabilities;
   end Print_Leaf;

   --  --------------------------------------------------------------------------

end Builder;
