--  Ref: https://github.com/random-forests/tutorials/blob/master/decision_tree.py

with Ada.Strings.Fixed;
with Ada.Text_IO; use Ada.Text_IO;

with Utilities;

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

   Header_Data  : Header_Data_Type;

   function Parse (aString : String) return Row_Data;
   function Parse_Header (Header : String) return Header_Data_Type;
   procedure Split (Rows     : Rows_Vector; Uncertainty : Float;
                    Question : in out Question_Data;
                    Best     : out Best_Data);
   function To_Boolean (Item : in Unbounded_String) return Boolean;
   function To_Float (Item : in Unbounded_String) return Float;
   function To_Integer (Item : in Unbounded_String) return Integer;
   function To_Label (UB_String : Raw_Label) return Label_Data;

   --  ------------------------------------------------------------------------

   procedure Best_Boolean_Value
     (Rows      : Rows_Vector; Value : Boolean;
      Feature   : Feature_Name_Type; Uncertainty : Float;
      Question  : in out Question_Data; Best : out Best_Data) is
   begin
      Question.Feature_Name := Feature;
      Question.Boolean_Value := Value;
      Split (Rows, Uncertainty, Question, Best);
   end Best_Boolean_Value;

   --  ------------------------------------------------------------------------

   procedure Best_Float_Value
     (Rows      : Rows_Vector; Value : Float;
      Feature   : Feature_Name_Type; Uncertainty : Float;
      Question  : in out Question_Data; Best : in out Best_Data) is
   begin
      Question.Feature_Name := Feature;
      Question.Float_Value := Value;
      Split (Rows, Uncertainty, Question, Best);
   end Best_Float_Value;

   --  -----------------------------------------------------------------------

   procedure Best_Integer_Value
     (Rows      : Rows_Vector; Value : Integer;
      Feature   : Feature_Name_Type; Uncertainty : Float;
      Question  : in out Question_Data; Best : in out Best_Data) is
   begin
      Question.Feature_Name := Feature;
      Question.Integer_Value := Value;
      Split (Rows, Uncertainty, Question, Best);
   end Best_Integer_Value;

   --  -----------------------------------------------------------------------

   function Best_Question (Data : Best_Data) return Question_Data is
   begin
      return Data.Question;
   end Best_Question;

   --  ------------------------------------------------------------------------

   procedure Best_String_Value
     (Rows      : Rows_Vector; Value : Unbounded_String;
      Feature   : Feature_Name_Type; Uncertainty : Float;
      Question  : out Question_Data; Best : in out Best_Data) is
   begin
      Question.Feature_Name := Feature;
      Question.UB_String_Value := Value;
      Split (Rows, Uncertainty, Question, Best);
   end Best_String_Value;

   --  ------------------------------------------------------------------------
   --  A Leaf node classifies data
   --  A Leaf node is a dictionary of classes  (features) (e.g., "Apple") and,
   --  for each class, the number of times that the class appears in the rows
   --  from the training data that reach this leaf.
   function Build_Tree (Rows : Rows_Vector) return Tree_Type is
      use Tree_Package;
      theTree   : Tree_Type := Empty_Tree;

      procedure Add_New_Decision_Node (Parent_Cursor : Tree_Cursor;
                                       Best_Split : Best_Data) is
         Node  : Tree_Node_Type (Decision_Kind);
      begin
         Node.Decision_Branch := True;
         Node.Question := Best_Split.Question;
         Node.True_Branch := Best_Split.True_Rows;
         Node.False_Branch := Best_Split.False_Rows;
         theTree.Insert_Child (Parent   => Parent_Cursor,
                               Before   => No_Element,
                               New_Item => Node);
      end Add_New_Decision_Node;

      procedure Add_Branch (Rows          : Rows_Vector;
                            Parent_Cursor : Tree_Cursor) is
         --  Parent_Cursor is a cursor to an existing node which is the head
         --  of this branch
         Best_Split       : constant Best_Data := Find_Best_Split (Rows);
         Leaf             : Tree_Node_Type (Prediction_Kind);
         Child_Cursor     : Tree_Cursor;
         True_Split_Rows  : Rows_Vector;
         False_Split_Rows : Rows_Vector;
      begin
         if Best_Split.Gain = 0.0 then
            New_Line;
            Leaf.Decision_Branch := False;
            Leaf.Prediction := Rows.First_Element;
            Leaf.Rows := Rows;
            Utilities.Print_Rows ("Prediction", Rows);
            New_Line;
            theTree.Insert_Child (Parent_Cursor, No_Element, Leaf);

         else
            Utilities.Print_Question ("Add_Branch Best split",
                                      Best_Split.Question);
            Add_New_Decision_Node (Parent_Cursor, Best_Split);
            True_Split_Rows := Best_Split.True_Rows;
            False_Split_Rows := Best_Split.False_Rows;
            Child_Cursor := Last_Child (Parent_Cursor);
            Utilities.Print_Rows ("Add_Branch True_Split_Rows", True_Split_Rows);
            Add_Branch (True_Split_Rows, Child_Cursor);
            Utilities.Print_Rows ("Add_Branch False_Split_Rows", False_Split_Rows);
            Add_Branch (False_Split_Rows, Child_Cursor);
            New_Line;
         end if;
      end Add_Branch;

   begin
      Add_Branch (Rows, theTree.Root);
      return theTree;

   end Build_Tree;

   --  ------------------------------------------------------------------------

   function Header_Row return Header_Data_Type is
   begin
      return Header_Data;
   end Header_Row;

   --  --------------------------------------------------------------------
   --  Uncertainty is the uncertainty of the starting node minus
   --  the weighted impurity of two child nodes
   function Information_Gain (Left, Right         : Rows_Vector;
                              Current_Uncertainty : Float) return float is
      Left_Length  : constant Float := Float (Left.Length);
      Right_Length : constant Float := Float (Right.Length);
      Prob_Left    : constant Float := Left_Length /
        (Left_Length + Right_Length);
   begin
      return Current_Uncertainty -
        Prob_Left * Gini (Left) - (1.0 - Prob_Left) * Gini (Right);
   end Information_Gain;

   --  ------------------------------------------------------------------------
   --  The root is a special node that is always present and has neither an
   --  associated element value nor any parent node.
   --  The root node provides a place to add nodes to an otherwise empty tree
   --  and represents the base of the tree.
   --  A cursor designates a particular node within a tree and, by extension,
   --  the element contained in that node, if any.
   --  A cursor keeps designating the same node (and element) as long as the
   --  node is part of the container even if the node is moved within the
   --  container.

   function Classify (aRow : Row_Data; Node_Cursor : Tree_Cursor)
                      return Prediction_Data_List is
      use Tree_Package;
      aNode      : constant Tree_Node_Type := Element (Node_Cursor);
      Prediction : Prediction_Data;
      Result     : Prediction_Data_List;
   begin
      if aNode.Node_Type = Prediction_Kind then
--           Put_Line ("Builder.Classify label: " &
--                       To_String (aNode.Prediction.Label));
         Prediction.Label := aNode.Prediction.Label;
         if Result.Contains (Prediction) then
            Prediction.Num_Copies := Prediction.Num_Copies + 1;
         else
            Prediction.Num_Copies := 1;
         end if;
         Result.Append (Prediction);
      else
         if Match (aNode.Question, aRow) then
            Result := Classify (aRow, First_Child (Node_Cursor));
         else
            Result := Classify (aRow, Last_Child (Node_Cursor));
         end if;
      end if;
      return Result;

   exception
      when others =>
         Put_Line ("Print_Classification exception");
         raise;
         return Result;
   end Classify;

   --  -----------------------------------------------------------------------
   --   Find_Best_Split finds the best question to ask by iterating over every
   --   feature / value and calculating the information gain.
   function Find_Best_Split (Rows : Rows_Vector) return Best_Data is
      use Rows_Package;
      Current_Uncertainty : constant Float := Gini (Rows);
      Cols                : constant Row_Data := Rows.First_Element;
      Row1                : constant Row_Data :=
                              Rows.Element (Rows.First_Index);
      Num_Features        : constant Class_Range := Cols.Class_Count;
      Row1_Features       : constant Feature_Data_Array (1 .. Num_Features) :=
                              Row1.Features;
      Feature_Value       : Unbounded_String;
      Feature_Name        : Feature_Name_Type;
      Feature_Data_Type   : Data_Type;
      Boolean_Question    : Question_Data (Boolean_Type);
      Integer_Question    : Question_Data (Integer_Type);
      Float_Question      : Question_Data (Float_Type);
      String_Question     : Question_Data (UB_String_Type);
      Best                : Best_Data;
   begin
      for col in 1 .. Num_Features loop
         Feature_Name := Feature_Name_Type (Header_Data.Features (col));
         Feature_Data_Type := Utilities.Get_Data_Type (Row1_Features (col));
         for row in
           Rows.First_Index .. Rows.Last_Index loop
            Feature_Value := Rows.Element (row).Features (col);
            case Feature_Data_Type is
               when Boolean_Type =>
                  Best_Boolean_Value
                    (Rows, To_Boolean (Feature_Value), Feature_Name,
                     Current_Uncertainty, Boolean_Question, Best);
               when Float_Type =>
                  Best_Float_Value
                    (Rows, To_Float (Feature_Value), Feature_Name,
                     Current_Uncertainty, Float_Question, Best);
               when Integer_Type =>
                  Best_Integer_Value
                    (Rows, To_Integer (Feature_Value), Feature_Name,
                     Current_Uncertainty, Integer_Question, Best);
               when UB_String_Type =>
                  Best_String_Value
                    (Rows, Feature_Value, Feature_Name, Current_Uncertainty,
                     String_Question, Best);
            end case;
         end loop;
      end loop;
      return Best;

   end Find_Best_Split;

   --  -------------------------------------------------------------------------

   function Gain (Data : Best_Data) return Float is
   begin
      return Data.Gain;
   end Gain;

   --  -------------------------------------------------------------------------

   function Gini (Rows : Rows_Vector) return Float is
      use UB_Label_Map_Package;
      Counts            : UB_Label_Map;
      Rows_Size         : constant Float := Float (Rows.Length);
      Impurity          : Float := 1.0;
      procedure Calc_Impurity (Curs : UB_Label_Map_Package.Cursor) is
         Label_Probability : Float range 0.0 .. 1.0;
      begin
         Label_Probability := Float (Element (Curs)) / Rows_Size;
         Impurity := Impurity - Label_Probability ** 2;
      end Calc_Impurity;
   begin
      Counts := UB_Label_Counts (Rows);
      Counts.Iterate (Calc_Impurity'Access);
      return Impurity;
   end Gini;

   --  ------------------------------------------------------------------------

   function Initialize (Rows : Data_Rows) return Rows_Vector is
      New_Vector  : Rows_Vector;
      First_Index : constant Positive := Rows'First;
      aRow        : Row_Data;
   begin
      Header_Data := Parse_Header (To_String (Rows (First_Index)));
      for index in Positive'Succ (First_Index) .. Rows'Last loop
         aRow := Parse (To_String (Rows (index)));
         New_Vector.Append (aRow);
      end loop;
      return New_Vector;
   end Initialize;

   --  --------------------------------------------------------------------------

   --  Match compares the feature value in an example to the
   --  feature value in a question.
   function Match (Question : Question_Data; Example_Data : Row_Data)
                   return Boolean is
      Feature_Name     : constant Feature_Name_Type := Question.Feature_Name;
      Feat_Index       : Class_Range;
      Example_Feature  : Unbounded_String;
      Val_Type         : Data_Type;
      Matches          : Boolean := False;
      Found            : Boolean := False;
   begin
      Val_Type  := Question.Feature_Kind;
      for col in Header_Data.Features'Range loop
         --              Put_Line ("Match Header_Data col: " & Class_Range'Image (col));
         if Feature_Name_Type (Header_Data.Features (col)) = Feature_Name then
            Feat_Index := col;
            Found := True;
         end if;
      end loop;

      --          if not Found then
      --              raise Builder_Exception with
      --              "Builder.Match, invalid feature Question.Feature_Name: " &
      --                To_String (Feature_Name);
      --          end if;

      --          Put_Line ("Match Header_Data Feat_Index: " & Class_Range'Image (Feat_Index));
      if Found then
         Example_Feature := Example_Data.Features (Feat_Index);
         case Val_Type is
            when Integer_Type =>
               declare
                  Value : constant Integer := Question.Integer_Value;
               begin
                  Matches := Value =
                    Integer'Value (To_String (Example_Feature));
               end;
            when Float_Type =>
               declare
                  Value : constant Float := Question.Float_Value;
               begin
                  Matches := Value =
                    Float'Value (To_String (Example_Feature));
               end;
            when Boolean_Type =>
               declare
                  Value : constant Boolean := Question.Boolean_Value;
               begin
                  Matches := Value =
                    Boolean'Value (To_String (Example_Feature));
               end;
            when UB_String_Type =>
               declare
                  Value : constant Unbounded_String := Question.UB_String_Value;
               begin
                  Matches := Value = Example_Feature;
               end;
         end case;
      end if;
      return Matches;
   end Match;

   --  ---------------------------------------------------------------------------

   function Num_Features (aString : String) return Class_Range is
      use Ada.Strings;
   begin
      return Class_Range (Fixed.Count (aString, ","));
   end Num_Features;

   --  ---------------------------------------------------------------------------

   function Parse (aString : String) return Row_Data is
      use Ada.Strings;
      Last         : constant Natural := aString'Length;
      Data_Row     : Row_Data (Num_Features (aString));
      Pos_1        : Natural := 0;
      Pos_2        : Natural;
   begin
      for index in 1 .. Num_Features (aString) loop
         Pos_2 := Fixed.Index (aString (Pos_1 + 1 .. Last) , ",");
         if Pos_2 > 0 then
            Data_Row.Features (index) :=
              To_Unbounded_String (aString (Pos_1 + 1 .. Pos_2 - 1));
            Pos_1 := Pos_2;
         end if;
      end loop;
      Data_Row.Label := To_Unbounded_String (aString (Pos_1 + 2 .. Last));
      return Data_Row;
   end Parse;

   --  ------------------------------------------------------------------------

   function Parse_Header (Header : String) return Header_Data_Type is
      use Ada.Strings;
      Num_Features : constant Class_Range :=
                       Class_Range (Fixed.Count (Header, ","));
      Last         : constant Natural := Header'Length;
      Header_Row   : Header_Data_Type (Num_Features);
      Pos_1        : Natural := Header'First;
      Pos_2        : Natural := Fixed.Index (Header (Pos_1 + 1 .. Last) , ",");
   begin
      for index in 1 .. Num_Features loop
         Pos_2 := Fixed.Index (Header (Pos_1 .. Last) , ",");
         Header_Row.Features (index) :=
           To_Unbounded_String (Header (Pos_1 .. Pos_2 - 1));
         Pos_1 := Pos_2 + 1;
      end loop;
      Header_Row.Label := To_Unbounded_String (Header (Pos_1 + 1 .. Last));
      return Header_Row;
   end Parse_Header;

   --  ---------------------------------------------------------------------------

   function Partition (Rows : Rows_Vector; aQuestion : Question_Data)
                       return Partitioned_Rows is
      True_Rows  : Rows_Vector;
      False_Rows : Rows_Vector;
      Data       : Row_Data;
   begin
      for index in Rows.First_Index .. Rows.Last_Index loop
         Data := Rows.Element (index);
         if Match (aQuestion, Data) then
            True_Rows.Append (Data);
         else
            False_Rows.Append (Data);
         end if;
      end loop;
      return (True_Rows, False_Rows);
   end Partition;

   --  ------------------------------------------------------------------------

   procedure Split (Rows     : Rows_Vector; Uncertainty : Float;
                    Question : in out Question_Data;
                    Best     : out Best_Data) is
      Empty_Row  :  Rows_Vector;
      Split_Rows :  Partitioned_Rows;
   begin
      Split_Rows := Partition (Rows, Question);
      if not Split_Rows.True_Rows.Is_Empty and then
        not Split_Rows.False_Rows.Is_Empty then
         Question.Gain := Information_Gain
           (Split_Rows.True_Rows, Split_Rows.False_Rows, Uncertainty);
         --   Floating point = is not reliable
         if Question.Gain >= Best.Gain then
            Best := (Question, Split_Rows.True_Rows, Split_Rows.False_Rows, Question.Gain);
         end if;
      elsif Best.Question.Feature_Name = To_Unbounded_String ("") then
         Best := (Question, Empty_Row, Empty_Row, 0.0);
      end if;
   end Split;

   --  --------------------------------------------------------------------------

   function To_Boolean (Item : in Unbounded_String) return Boolean is
   begin
      return Boolean'Value (To_String (Item));
   end To_Boolean;

   --  --------------------------------------------------------------------------

   function To_Float (Item : in Unbounded_String) return Float is
   begin
      return Float'Value (To_String (Item));
   end To_Float;

   --  --------------------------------------------------------------------------

   function To_Integer (Item : in Unbounded_String) return Integer is
   begin
      return Integer'Value (To_String (Item));
   end To_Integer;

   --  --------------------------------------------------------------------------

   function To_Label (UB_String : Raw_Label) return Label_Data is
      use Utilities;
      Value : constant String := To_String (UB_String);
      Label : Label_Data;
   begin
      if Is_Integer (UB_String) then
         declare
            Label_I  : Label_Data (Integer_Type);
         begin
            Label_I.Integer_Value := Integer'Value (Value);
            Label :=  Label_I;
         end;
      elsif Is_Float (UB_String) then
         declare
            Label_F  : Label_Data (Float_Type);
         begin
            Label_F.Float_Value := Float'Value (Value);
            Label := Label_F;
         end;
      elsif Is_Boolean (UB_String) then
         declare
            Label_B  : Label_Data (Boolean_Type);
         begin
            Label_B.Boolean_Value := Boolean'Value (Value);
            Label := Label_B;
         end;
      else
         declare
            Label_UB  : Label_Data (UB_String_Type);
         begin
            Label_UB.UB_String_Value := UB_String;
            Label := Label_UB;
         end;
      end if;

      return Label;
   end To_Label;

   --  --------------------------------------------------------------------------

   function To_Question (Q : Raw_Question) return Question_Data is
      use Utilities;
      Value  : constant String := To_String (Q.Feature_Value);
      Q_Data : Question_Data;
   begin
      if Is_Integer (Q.Feature_Value) then
         declare
            QD : Question_Data (Integer_Type);
         begin
            QD.Integer_Value := Integer'Value (Value);
            Q_Data := QD;
         end;
      elsif Is_Float (Q.Feature_Value) then
         declare
            QD : Question_Data (Float_Type);
         begin
            QD.Float_Value := Float'Value (Value);
            Q_Data := QD;
         end;
      elsif Is_Boolean (Q.Feature_Value) then
         declare
            QD : Question_Data (Boolean_Type);
         begin
            QD.Boolean_Value := Boolean'Value (Value);
            Q_Data := QD;
         end;
      else
         declare
            QD : Question_Data (UB_String_Type);
         begin
            QD.UB_String_Value := To_Unbounded_String (Value);
            Q_Data := QD;
         end;
      end if;

      Q_Data.Feature_Name := Q.Feature_Name;
      return Q_Data;

   end To_Question;

   --  --------------------------------------------------------------------------
   --  Based on class_counts(rows)
   --  UB_Label_Counts counts the number of each type of example in a dataset.
   function UB_Label_Counts (Rows : Rows_Vector) return UB_Label_Map is
      use Rows_Package;
      use UB_Label_Map_Package;
      Label_Counts : UB_Label_Map;
      Count_Cursor : UB_Label_Map_Package.Cursor;
      aRow         : Row_Data;
      Label        : Label_Data;
      Count        : Natural := 0;
   begin
      for index in Rows.First_Index .. Rows.Last_Index loop
         aRow := Rows.Element (index);
         Label := To_Label (aRow.Label);
         if Label.Label_Kind /= UB_String_Type then
            raise Builder_Exception with
              "Builder.UB_Class_Counts, Label_Kind is not UB_String_Type";
         else
            if not Label_Counts.Contains (Label.UB_String_Value) then
               Label_Counts.Insert (Label.UB_String_Value, 0);
            end if;
            Count_Cursor := Label_Counts.Find (Label.UB_String_Value);
            Count := Label_Counts.Element (Label.UB_String_Value);
            Count := Count + 1;
            Label_Counts.Replace_Element (Count_Cursor, Count);
         end if;
      end loop;
      return Label_Counts;
   end UB_Label_Counts;

   --  ---------------------------------------------------------------------------

end Builder;
