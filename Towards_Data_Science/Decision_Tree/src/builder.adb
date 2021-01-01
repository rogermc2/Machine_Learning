
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
      aLeaf        : Decision_Node_Type (Prediction_Kind);

      procedure Recurse (Rows : Rows_Vector;
                         Curs : in out Tree_Cursor) is
         use Support;
         P_Rows       : Partitioned_Rows;
         Node         : Decision_Node_Type;
         Node_Curs    : Tree_Cursor;
      begin
         Best_Split := Find_Best_Split (Rows);
         if Best_Split.Best_Gain = 0.0 then
            aLeaf.Predictions := Support.Class_Counts (Rows);
            Put_Line ("Predictions" & Integer'Image (aLeaf.Predictions.First_Element));
            theTree.Insert_Child (Parent   => Curs,
                                  Before   => No_Element,
                                  New_Item => aLeaf);
         else
            P_Rows := Partition (Rows, Best_Split.Best_Question);
            Node.Question := Best_Split.Best_Question;
            Node.True_Rows := P_Rows.True_Rows;
            Node.False_Rows := P_Rows.False_Rows;
            Print_Rows ("Node.True_Rows", Node.True_Rows);
            Print_Rows ("Node.False_Rows", Node.False_Rows);
            theTree.Insert_Child (Parent   => Curs,
                                  Before   => No_Element,
                                  New_Item => Node,
                                  Position => Node_Curs);
            Recurse (P_Rows.True_Rows, Node_Curs);
            Recurse (P_Rows.False_Rows, Node_Curs);
         end if;
         New_Line;
      end Recurse;

   begin
      New_Line;
      Put_Line ("Build Tree");
      Recurse (Rows, Root_Curs);
      Put_Line ("Tree built");
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
      Subtree          : Tree_Type;
      Sub_Cursor       : constant Cursor := Root (Subtree);
      Result           : Count_Package.Map;
   begin
      if Is_Leaf (First_Cursor) then
--           Put_Line ("Classify Leaf");
         Result := First_Node.Predictions;
      else
         case First_Node.Node_Type is
         when Prediction_Kind =>
            null;
         when Decision_Kind =>
--              Put ("Decision_Kind ");
            if Match (First_Node.Question, aRow) then
--                 Put_Line ("Match");
               Subtree.Copy_Subtree (Sub_Cursor, No_Element, True_Cursor);
            else
--                 Put_Line ("No match");
               Subtree.Copy_Subtree (Sub_Cursor, No_Element, False_Cursor);
            end if;
            Result := Classify (aRow, Subtree);
         end case;
      end if;
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
         Colour_Question.Colour_Value := Colour_Values (col);
         Dimension_Question.Diameter_Value := Dimension_Values (col);
         Split_Row := Partition (Rows, Colour_Question);
         Test_Gain;
         Split_Row := Partition (Rows, Dimension_Question);
         Test_Gain;
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

   procedure Print_Classification (Classification : Count_Package.Map) is
      use Count_Package;
      aCount : Natural;
   begin
      Put_Line ("Class_Counts:");
      for index in Classification.First_Key .. Classification.Last_Key loop
         aCount := Classification.Element (index);
         Put_Line (Label_Type'Image (index) &  ": " & Natural'Image (aCount));
      end loop;
   end Print_Classification;

   --  --------------------------------------------------------------------------

   procedure Print_Class_Counts (Rows : Rows_Vector) is
      use Count_Package;
      Counts : constant Count_Package.Map := Support.Class_Counts (Rows);
      aCount : Natural;
   begin
      Put_Line ("Class_Counts:");
      for index in Counts.First_Key .. Counts.Last_Key loop
         aCount := Counts.Element (index);
         Put_Line (Label_Type'Image (index) &  ": " & Natural'Image (aCount));
      end loop;
   end Print_Class_Counts;

   --  --------------------------------------------------------------------------

   function Print_Leaf (Counts : Count_Package.Map) return Strings_List is
      use Count_Package;
      Total         : Natural := 0;
      aCount        : Natural;
      aString       : Unbounded_String;
      Probabilities : Strings_List;
   begin
      Put_Line ("Counts size:" & Natural'Image (Natural (Counts.Length)));
      for index in Counts.First_Key .. Counts.Last_Key loop
         Total := Total + Counts.Element (index);
         Put_Line ("Total:" & Natural'Image (Total));
      end loop;
      Put_Line ("Probabilities:");
      for index in Counts.First_Key .. Counts.Last_Key loop
         aCount := Counts.Element (index);
         Put_Line ("aCount:" & Natural'Image (aCount));
         aString :=
           To_Unbounded_String (Natural'Image ((100 * aCount) / Total)) & "%";
         Probabilities.Append (aString);
         Put_Line (To_String (aString));
      end loop;
      return Probabilities;
   end Print_Leaf;

   --  --------------------------------------------------------------------------

   procedure Print_Unique_Values (Rows    : Rows_Vector;
                                  Feature : Feature_Type) is
      use Value_Set_Package;
      Values : constant Value_Set := Unique_Values (Rows, Feature);
      Curs   : Cursor := Values.First;
      Data   : Value_Data (Feature);
   begin
      Put ("Unique " & Feature_Type'Image (Feature)  & " Values:");
      while Has_Element (Curs) loop
         case Feature is
            when Colour_Feature =>
               Data := Element (Curs);
               Put (" " & Colour_Type'Image (Data.Colour));
            when Diameter_Feature  =>
               Data := Element (Curs);
               Put (Integer'Image (Data.Diameter) & " ");
         end case;
         Next (Curs);
      end loop;
      New_Line;
   end Print_Unique_Values;

   --  --------------------------------------------------------------------------

   function Unique_Values (Rows    : Rows_Vector;
                           Feature : Feature_Type) return Value_Set is
      Data   : Row_Data;
      Value  : Value_Data (Feature);
      theSet : Value_Set;
   begin

      for index in Rows.First_Index .. Rows.Last_Index loop
         Data := Rows.Element (index);
         if Feature = Colour_Feature then
            Value.Colour := Data.Colour;
         else
            Value.Diameter := Data.Diameter;
         end if;
         if not theSet.Contains (Value) then
            theSet.Append (Value);
         end if;
      end loop;
      return theSet;
   end Unique_Values;

   --  --------------------------------------------------------------------------

end Builder;
