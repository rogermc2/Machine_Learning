
with Ada.Containers;
with Ada.Text_IO; use Ada.Text_IO;

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

   --     type Node;
   --     type Tree is access Node;
   --     type Node is record
   --        The_Item : INTEGER;
   --        Left_Subtree : Tree;
   --        Right_Subtree: Tree;
   --     end record;

   --  -----------------------------------------------------------------------
   --  A Leaf node classifies data.
   --  A Leaf node is a dictionary of classes  (features) (e.g., "Apple") and,
   --  for each class, the number of times that the class appears in the rows
   --  from the training data that reach this leaf.
   function Build_Tree (Rows : Support.Rows_Vector) return Tree_Type is
      use Support;
      use Tree_Package;
      theTree      : Tree_Type := Empty_Tree;
      Root_Curs    : Tree_Cursor := Root (theTree);
      Best_Split   : Best_Split_Data;
      True_Branch  : Decision_Node_Type;
      False_Branch : Decision_Node_Type;
      aLeaf        : Decision_Node_Type (Prediction_Kind);

      procedure Recurse (Rows : Support.Rows_Vector;
                         Curs : in out Tree_Cursor) is
         P_Rows       : Partitioned_Rows;
         True_Curs    : Tree_Cursor;
         False_Curs   : Tree_Cursor;
      begin
         Put_Line ("Recursing, Rows length: " &
                     Integer'Image (Integer (Rows.Length)));
         Best_Split := Find_Best_Split (Rows);
         if Best_Split.Best_Gain = 0.0 then
            Put_Line ("Best_Gain = 0.0");
            aLeaf.Predictions := Class_Counts (Rows);
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

   function Classify (aRow : Support.Row_Data; aTree : Tree_Type)
                      return Support.Count_Package.Map is
      use Tree_Package;
      Root_Cursor   : constant Tree_Cursor := aTree.Root;
      True_Cursor   : constant Tree_Cursor := First_Child (Root_Cursor);
      False_Cursor  : constant Tree_Cursor := Last_Child (Root_Cursor);
      Node          : constant Decision_Node_Type := Element (Root_Cursor);
      True_Subtree  : Tree_Type;
      False_Subtree : Tree_Type;
      Result        : Support.Count_Package.Map;
   begin
      True_Subtree.Copy_Subtree (Root_Cursor, No_Element, True_Cursor);
      False_Subtree.Copy_Subtree (Root_Cursor, No_Element, False_Cursor);
      case Node.Node_Type is
         when Prediction_Kind =>
            --  Leaf
            Result := Node.Predictions;
         when Decision_Kind =>
            if Match (Node.Question, aRow) then
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
   function Find_Best_Split (Rows : Support.Rows_Vector)
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

   function Match (Self : Support.Question_Type; Example : Support.Row_Data)
                   return Boolean is
      use Support;
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

   function Partition (Rows     : Support.Rows_Vector;
                       Question : Support.Question_Type)
                       return Partitioned_Rows is
      use Support;
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

   procedure Print_Tree1 (Node : Decision_Node_Type) is
      use Support;
      Question   : constant Support.Question_Type := Node.Question;
      --        Counts     : constant Support.Count_Package.Map := Node.Counts;
   begin
      New_Line;
      Put_Line ("Tree:");
      Put (Feature_Type'Image (Question.Feature) & "  ");
      case Question.Feature is
         when Colour_Feature =>
            New_Line;
            --              Put_Line (Colour_Type'Image (Question.Colour_Value));
         when Diameter_Feature =>
            Put_Line (Integer'Image (Question.Diameter_Value));
      end case;
      Print_Rows ("True rows", Node.True_Rows);
      Print_Rows ("False rows", Node.False_Rows);
      New_Line;

   end Print_Tree1;

   --  --------------------------------------------------------------------------

   procedure Print_Tree (aTree : Tree_Type) is
      use Support;
      use Tree_Package;
      Level       : Integer := 0;
      Left_Level  : Integer := 0;
      Right_Level : Integer := 0;
      --        Counts     : constant Support.Count_Package.Map := Node.Counts;
      procedure Recurse (Curs : Tree_Cursor) is
         Node       : constant Decision_Node_Type := Element (Curs);
         Question   : Support.Question_Type;
      begin
         Level := Level + 1;
         Put_Line ("Level:" & Integer'Image (Level));
         case Node.Node_Type is
            when Decision_Kind =>
               Question := Node.Question;
               Put (Feature_Type'Image (Question.Feature) & "  ");
               case Node.Question.Feature is
               when Colour_Feature =>
                  Put_Line (Colour_Type'Image (Question.Colour_Value));
               when Diameter_Feature =>
                  Put_Line (Integer'Image (Question.Diameter_Value));
               end case;
               Print_Rows ("True_Rows", Node.True_Rows);
               Print_Rows ("False_Rows", Node.False_Rows);
            when Prediction_Kind =>
               null;
         end case;

         if First_Child (Curs) /= Tree_Package.No_Element then
            New_Line;
            Left_Level := Left_Level + 1;
            Put_Line ("Left_Level:" & Integer'Image (Left_Level));
            Recurse (First_Child (Curs));
         else
            Put_Line
              ("Left leaf reached at left level" & Integer'Image (Left_Level));
         end if;
         if Next_Sibling (Curs) /= Tree_Package.No_Element then
            New_Line;
            Right_Level := Right_Level + 1;
            Put_Line ("Right_Level:" & Integer'Image (Right_Level));
            Recurse (Next_Sibling (Curs));
         else
            Right_Level := Right_Level + 1;
            Put_Line
              ("Right leaf reached at right level" & Integer'Image (Right_Level));
         end if;
      end Recurse;
   begin
      New_Line;
      Put_Line ("Tree:");
      Recurse (First_Child (Root (aTree)));
      New_Line;

   end Print_Tree;

   --  --------------------------------------------------------------------------

end Builder;
