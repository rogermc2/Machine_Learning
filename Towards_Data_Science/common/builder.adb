--  Ref: https://github.com/random-forests/tutorials/blob/master/decision_tree.py

with Ada.Characters.Handling;
with Ada.Containers;
with Ada.Strings.Fixed;
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

   --     type Header_Type is array (Integer range <>) of Unbounded_String;
   --     Header : constant Header_Type (1 .. 3) :=
   --                (To_Unbounded_String ("Colour"),
   --                 To_Unbounded_String ("Diameter"),
   --                 To_Unbounded_String ("Label"));

   pragma Warnings (Off, "procedure ""Print_Row"" is not referenced");

   Features      : Feature_Map;
   --     Feature_Types : Feature_Type_Map;
   Label_Types   : Label_Type_Map;

   function Get_Data_Type (Data : Unbounded_String) return Data_Type;
   function Is_Boolean (Item : in Unbounded_String) return Boolean;
   function Is_Float (Item : in Unbounded_String) return Boolean;
   function Is_Integer (Item : in Unbounded_String) return Boolean;
   procedure Print_Row (Label : String; Row : Row_Data);
   procedure Set_Feature_Map (Features_Array : Feature_Names);
   --     procedure Set_Feature_ID (Feature : String; Feat_ID : Class_Range);
   function To_Boolean (Item : in Unbounded_String) return Boolean;
   function To_Float (Item : in Unbounded_String) return Float;
   function To_Integer (Item : in Unbounded_String) return Integer;
   function To_Label (UB_String : Raw_Label) return Label_Data;
   --     function To_Row_Data (aRow : Unbounded_String) return Row_Data;

   --  -----------------------------------------------------------------------
   --  A Leaf node classifies data.
   --  A Leaf node is a dictionary of classes  (features) (e.g., "Apple") and,
   --  for each class, the number of times that the class appears in the rows
   --  from the training data that reach this leaf.
   --     function Build_Tree (Rows : Rows_Vector) return Tree_Type is
   --        use Tree_Package;
   --        theTree      : Tree_Type := Empty_Tree;
   --        Root_Curs    : Tree_Cursor := Root (theTree);
   --        Best_Split   : Best_Split_Data;
   --        aLeaf        : Decision_Node_Type (Prediction_Kind);
   --
   --        procedure Recurse (Rows : Rows_Vector;
   --                           Curs : in out Tree_Cursor) is
   --           P_Rows       : Partitioned_Rows;
   --           Node         : Decision_Node_Type;
   --           Node_Curs    : Tree_Cursor;
   --        begin
   --           Best_Split := Find_Best_Split (Rows);
   --           if Best_Split.Best_Gain = 0.0 then
   --              aLeaf.Predictions := Class_Counts (Rows);
   --              Put_Line ("Predictions" & Integer'Image (aLeaf.Predictions.First_Element));
   --              theTree.Insert_Child (Parent   => Curs,
   --                                    Before   => No_Element,
   --                                    New_Item => aLeaf);
   --           else
   --              P_Rows := Partition (Rows, Best_Split.Best_Question);
   --              Node.Question := Best_Split.Best_Question;
   --              Node.True_Rows := P_Rows.True_Rows;
   --              Node.False_Rows := P_Rows.False_Rows;
   --              Print_Rows ("Node.True_Rows", Node.True_Rows);
   --              Print_Rows ("Node.False_Rows", Node.False_Rows);
   --              theTree.Insert_Child (Parent   => Curs,
   --                                    Before   => No_Element,
   --                                    New_Item => Node,
   --                                    Position => Node_Curs);
   --              Recurse (P_Rows.True_Rows, Node_Curs);
   --              Recurse (P_Rows.False_Rows, Node_Curs);
   --           end if;
   --           New_Line;
   --        end Recurse;
   --
   --     begin
   --        New_Line;
   --        Put_Line ("Build Tree");
   --        Recurse (Rows, Root_Curs);
   --        Put_Line ("Tree built");
   --        return theTree;
   --     end Build_Tree;

   --  ---------------------------------------------------------------------------

   --  ---------------------------------------------------------------------------
   --  Uncertainty is the uncertainty of the starting node minus
   --  the weighted impurity of two child nodes
   function Information_Gain (Left, Right         : Rows_Vector;
                              Current_Uncertainty : Float) return float is
      Left_Length : constant Float := Float (Left.Length);
      P           : constant Float := Left_Length /
        (Left_Length + Float (Right.Length));
   begin
      return Current_Uncertainty -
        P * Gini (Left) - (1.0 - P) * Gini (Right);
   end Information_Gain;

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
   --     function Classify (aRow : Row_Data; aTree : Tree_Type)
   --                        return Count_Package.Map is
   --        use Tree_Package;
   --        --  The node at the root has no element so hence the call of First_Child.
   --        First_Cursor     : constant Cursor := First_Child (Root (aTree));
   --        True_Cursor      : constant Cursor := First_Child (First_Cursor);
   --        False_Cursor     : constant Cursor := Last_Child (First_Cursor);
   --        First_Node       : constant Decision_Node_Type := Element (First_Cursor);
   --        Subtree          : Tree_Type;
   --        Sub_Cursor       : constant Cursor := Root (Subtree);
   --        Result           : Count_Package.Map;
   --     begin
   --        if Is_Leaf (First_Cursor) then
   --  --           Put_Line ("Classify Leaf");
   --           Result := First_Node.Predictions;
   --        else
   --           case First_Node.Node_Type is
   --           when Prediction_Kind =>
   --              null;
   --           when Decision_Kind =>
   --  --              Put ("Decision_Kind ");
   --              if Match (First_Node.Question, aRow) then
   --  --                 Put_Line ("Match");
   --                 Subtree.Copy_Subtree (Sub_Cursor, No_Element, True_Cursor);
   --              else
   --  --                 Put_Line ("No match");
   --                 Subtree.Copy_Subtree (Sub_Cursor, No_Element, False_Cursor);
   --              end if;
   --              Result := Classify (aRow, Subtree);
   --           end case;
   --        end if;
   --        return Result;
   --
   --     exception
   --           when others =>
   --           Put_Line ("Print_Classification exception");
   --           raise;
   --        return Result;
   --     end Classify;

   --  -----------------------------------------------------------------------

   --     procedure Evaluate (Rows : Rows_Vector; theTree : Tree_Type) is
   --        aRow           : Row_Data;
   --        Classification : Count_Package.Map;
   --     begin
   --        for row in Rows.First_Index .. Rows.Last_Index loop
   --           aRow := Rows.Element (row);
   --           Put_Line ("Evalution of row " & Integer'Image (row));
   --           Classification := Classify (aRow, theTree);
   --  --           Put_Line ("Actual: " & Label_Type'Image (aRow.Fruit) & "  Predicted: " &
   --  --                       Print_Leaf (Classification));
   --        end loop;
   --     end Evaluate;

   --  -----------------------------------------------------------------------
   --   Find_Best_Split finds the best question to ask by iterating over every
   --   feature / value and calculating the information gain.
   function Find_Best_Split (Rows : Rows_Vector)
                             return Best_Split_Data is
      use Ada.Containers;
      use Rows_Package;
      Current_Uncertainty   : constant Float := Gini (Rows);
      Cols                  : constant Row_Data := Rows.First_Element;
      Row2                  : constant Row_Data :=
                                Rows.Element (Positive'Succ (Rows.First_Index));
      Label_Array           : array (1 .. Integer (Rows.Length) - 1) of
        Unbounded_String;
      Label_Index           : Natural := 0;
      Label                 : Unbounded_String;
      Num_Features          : constant Class_Range := Cols.Class_Count;
      Row2_Features         : constant Feature_Data (1 .. Num_Features) :=
                                Row2.Features;
      Features              : Feature_Data (1 .. Num_Features);
      Feature               : Unbounded_String;
      Feature_Data_Type     : Data_Type;
      Boolean_Values        : array (1 .. Num_Features) of Boolean;
      Float_Values          : array (1 .. Num_Features) of Float;
      Integer_Values        : array (1 .. Num_Features) of Integer;
      String_Values         : array (1 .. Num_Features) of Unbounded_String;
      Boolean_Question      : Question_Data (Boolean_Type);
      Integer_Question      : Question_Data (Integer_Type);
      Float_Question        : Question_Data (Float_Type);
      String_Question       : Question_Data (UB_String_Type);
      aRaw_Question         : Raw_Question;
      Split_Row             : Partitioned_Rows;
      Gain                  : Float := 0.0;
      Best_Gain             : Float := 0.0;
      Question_Data_Type    : Data_Type;
      theQuestion_Type      : Question_Type;
      Best_Question_Type    : Question_Type;
      Best_Q_Data_Type      : Data_Type;
      Best_Boolean_Question : Question_Data (Boolean_Type);
      Best_Float_Question   : Question_Data (Float_Type);
      Best_Integer_Question : Question_Data (Integer_Type);
      Best_String_Question  : Question_Data (UB_String_Type);
      String_Feature        : Unbounded_String;
      Result                : Best_Split_Data;
   begin
      --  Rows.First_Index contains column names
      if Rows.Length < 2 then
         raise Builder_Exception with
           "Builder.Find_Best_Split called with empty rows vector";
      else
         for row in
           Positive'Succ (Rows.First_Index) .. Rows.Last_Index loop
            Label_Index := Label_Index + 1;
            Label_Array (Label_Index) := Rows.Element (row).Label;
         end loop;

         for col in 1 .. Num_Features loop
            Feature_Data_Type := Get_Data_Type (Row2_Features (col));
            for row in
              Positive'Succ (Rows.First_Index) .. Rows.Last_Index loop
               Feature := Rows.Element (row).Features (col);
               case Feature_Data_Type is
                  when Boolean_Type =>
                     Boolean_Values (col) := To_Boolean (Feature);
                  when Float_Type =>
                     Float_Values (col) := To_Float (Feature);
                  when Integer_Type =>
                     Integer_Values (col) := To_Integer (Feature);
                  when UB_String_Type =>
                     String_Values (col) := Feature;
               end case;
            end loop;

            case Feature_Data_Type is
               when Boolean_Type =>
                  for val in Boolean_Values'Range loop
                     Boolean_Question.Feature := Feature;
                     Boolean_Question.Boolean_Value := Boolean_Values (val);
                     Split_Row := Partition (Rows, Boolean_Question);
                     if not Split_Row.True_Rows.Is_Empty and then
                       not Split_Row.False_Rows.Is_Empty then
                        Boolean_Question.Gain := Information_Gain
                          (Split_Row.True_Rows, Split_Row.False_Rows,
                           Current_Uncertainty);
                        if Boolean_Question.Gain >= Best_Gain then
                           Best_Gain := Boolean_Question.Gain;
                           Best_Boolean_Question := Boolean_Question;
                        end if;
                     end if;
                  end loop;
               when Float_Type =>
                  Float_Values (col) := To_Float (Feature);
               when Integer_Type =>
                  Integer_Values (col) := To_Integer (Feature);
               when UB_String_Type =>
                  String_Values (col) := Feature;
            end case;
         end loop;

         --           for row in Positive'Succ (Rows.First_Index) .. Rows.Last_Index loop
         --              Cols := Rows.Element (row);
         --              Features := Cols.Features;
         --              Label := Cols.Label;
         --
         --              for col in 1 .. Num_Features loop
         --                 String_Feature := Features (col);
         --                 aRaw_Question.Value := Label;
         --                 aRaw_Question.Feature := String_Feature;
         --                 declare
         --                    Boolean_Feature : Boolean;
         --                    Float_Feature   : Float;
         --                    Integer_Feature : Integer;
         --                 begin
         --                    if Is_Boolean (String_Feature) then
         --                       Boolean_Feature := To_Boolean (String_Feature);
         --                       Question_Data_Type := Boolean_Type;
         --  --                       theQuestion_Type := Boolean_Question;
         --                    elsif Is_Float (String_Feature) then
         --                       Float_Feature := To_Float (String_Feature);
         --  --                       theQuestion_Type := Float_Question;
         --                       Question_Data_Type := Float_Type;
         --                    elsif Is_Integer (String_Feature) then
         --                       Integer_Feature := To_Integer (String_Feature);
         --  --                       theQuestion_Type := Integer_Question;
         --                       Question_Data_Type := Integer_Type;
         --                    else --  should be a string
         --  --                       theQuestion_Type := String_Question;
         --                       Question_Data_Type := UB_String_Type;
         --                    end if;
         --
         --                    declare
         --                       aQuestion : Question_Data (Question_Data_Type);
         --                       procedure Test_Gain is
         --                          Gain : Float := 0.0;
         --                       begin
         --                          if Split_Row.True_Rows.Length /= 0 and then
         --                            Split_Row.False_Rows.Length /= 0 then
         --                             Gain := Information_Gain
         --                               (Split_Row.True_Rows, Split_Row.False_Rows,
         --                                Current_Uncertainty);
         --                             if Gain > Best_Gain then
         --                                Best_Gain := Gain;
         --                                case Best_Q_Data_Type is
         --                                   when Boolean_Type =>
         --                                      Best_Boolean_Question := aQuestion;
         --                                   when Float_Type =>
         --                                      Best_Float_Question := aQuestion;
         --                                   when Integer_Type =>
         --                                      Best_Integer_Question := aQuestion;
         --                                   when UB_String_Type =>
         --                                      Best_String_Question := aQuestion;
         --                                end case;
         --                             end if;
         --                          end if;
         --                       end Test_Gain;
         --
         --                    begin
         --                       aQuestion.Feature := Features (col);
         --                       case Question_Data_Type is
         --                          when Integer_Type =>
         --                             aQuestion.Integer_Value := Integer_Feature;
         --                          when Float_Type =>
         --                             aQuestion.Float_Value := Float_Feature;
         --                          when Boolean_Type =>
         --                             aQuestion.Boolean_Value := Boolean_Feature;
         --                          when UB_String_Type =>
         --                             aQuestion.UB_String_Value := String_Feature;
         --                       end case;
         --
         --                       Split_Row := Partition (Rows, aQuestion);
         --                       Test_Gain;
         --                    end;
         --                 end;  --  declare block
         --              end loop;
         --           end loop;
      end if;
      case Best_Q_Data_Type is
         when Boolean_Type =>
            Result := (Best_Gain, Best_Question_Type);
         when Float_Type =>
            Result := (Best_Gain, Best_Question_Type);
         when Integer_Type =>
            Result := (Best_Gain, Best_Question_Type);
         when UB_String_Type =>
            Result := (Best_Gain, Best_Question_Type);
      end case;

      return Result;
   end Find_Best_Split;

   --  ---------------------------------------------------------------------------

   function Get_Data_Type (Data : Unbounded_String) return Data_Type is
      theType : Data_Type;
   begin
      if Is_Integer (Data) then
         theType := Integer_Type;
      elsif Is_Float (Data) then
         theType := Float_Type;
      elsif Is_Boolean (Data) then
         theType := Boolean_Type;
      else
         theType := UB_String_Type;
      end if;

      return theType;
   end Get_Data_Type;

   --  ---------------------------------------------------------------------------

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
      Print_Rows ("Gini Rows", Rows);
      Counts := UB_Class_Counts (Rows);
      Print_UB_Class_Counts (Rows);
      Counts.Iterate (Calc_Impurity'Access);
      return Impurity;
   end Gini;

   function Is_Boolean (Item : in Unbounded_String) return Boolean is
      Item_String : constant String :=
                      Ada.Characters.Handling.To_Upper (To_String (Item));
   begin
      return Item_String = "TRUE" or else Item_String = "FALSE";
   end Is_Boolean;

   --  ---------------------------------------------------------------------------

   function Is_Float (Item : in Unbounded_String) return Boolean is
      Item_String : constant String := To_String (Item);
      use Ada.Strings;
   begin
      return Fixed.Count (Item_String, ".") = 1;
   end Is_Float;

   --  ---------------------------------------------------------------------------

   function Is_Integer (Item : in Unbounded_String) return Boolean is
      Item_String : constant String := To_String (Item);
      Dig         : Boolean := True;
   begin
      for index in Item_String'Range loop
         Dig := Dig and then
           Ada.Characters.Handling.Is_Decimal_Digit (Item_String (index));
      end loop;
      return Dig;
   end Is_Integer;

   --  ---------------------------------------------------------------------------
   --  Match compares the feature value in an example to the
   --  feature value in a question.
   function Match (Self : Question_Data; Example : Row_Data) return Boolean is
      Col       : constant Unbounded_String := Self.Feature;
      Feat_Type : constant Class_Range := Features.Element (Col);
      Val_Type  : constant Data_Type := Self.Feature_Kind;
      Matches   : Boolean := False;
   begin
      --        New_Line;
      --        Put_Line ("Match, Col: " & To_String (Col));
      --        Put_Line ("Match, Value type: " & Data_Type'Image (Val_Type));
      --        Put_Line ("Feat_Type: " & Class_Range'Image (Feat_Type) &
      --                    " ("  & To_String (Col) & ")");
      case Val_Type is
         when Integer_Type =>
            declare
               Value : constant Integer := Self.Integer_Value;
            begin
               Matches := Value =
                 Integer'Value (To_String (Example.Features (Feat_Type)));
            end;
         when Float_Type =>
            declare
               Value : constant Float := Self.Float_Value;
            begin
               Matches := Value =
                 Float'Value (To_String (Example.Features (Feat_Type)));
            end;
         when Boolean_Type =>
            declare
               Value : constant Boolean := Self.Boolean_Value;
            begin
               Matches := Value =
                 Boolean'Value (To_String (Example.Features (Feat_Type)));
            end;
         when UB_String_Type =>
            declare
               Value : constant Unbounded_String := Self.UB_String_Value;
            begin
               Matches := Value = Example.Features (Feat_Type);
            end;
      end case;
      --        Put_Line ("Example feature type : " & To_String (Example.Features (Feat_Type)));
      --        Put_Line ("Match: " & Boolean'Image (Matches));
      return Matches;
   end Match;

   --  ---------------------------------------------------------------------------

   function Num_Features (aString : String) return Class_Range is
      use Ada.Strings;
   begin
      --      : constant Class_Range :=
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
      Data_Row.Label := To_Unbounded_String (aString (Pos_1 + 1 .. Last));
      return Data_Row;
   end Parse;

   --  ---------------------------------------------------------------------------

   function Parse_Header (Header : String) return Header_Data is
      use Ada.Strings;
      Num_Features : constant Class_Range :=
                       Class_Range (Fixed.Count (Header, ","));
      Last         : constant Natural := Header'Length;
      Header_Row   : Header_Data (Num_Features);
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
      Set_Feature_Map (Header_Row.Features);
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
         --           Print_Row ("Partition a row", Data);
         if Match (aQuestion, Data) then
            --              Put_Line ("Partition matched");
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
      Put_Line ("Classification:");
      for index in Classification.First_Key .. Classification.Last_Key loop
         if Classification.Contains (index) then
            aCount := Classification.Element (index);
            Put_Line (Data_Type'Image (index) &  ": " & Natural'Image (aCount));
         else
            Put_Line (Data_Type'Image (index) &  ": none");
         end if;
      end loop;

   exception
      when others =>
         Put_Line ("Print_Classification exception");
         raise;
   end Print_Classification;

   --  --------------------------------------------------------------------------

   procedure Print_UB_Class_Counts (Rows : Rows_Vector) is
      use UB_Label_Map_Package;
      Counts       : constant UB_Label_Map := UB_Class_Counts (Rows);
      Count_Cursor : UB_Label_Map_Package.Cursor := Counts.First;
      aCount       : Natural;
   begin
      Put_Line ("Class_Counts:");
      while Has_Element (Count_Cursor) loop
         aCount := Element (Count_Cursor);
         Put_Line (To_String ((Key (Count_Cursor))) &  ": " & Natural'Image (aCount));
         next (Count_Cursor);
      end loop;
   end Print_UB_Class_Counts;

   --  --------------------------------------------------------------------------

   function Print_Leaf (Counts : Count_Package.Map) return String is
      use Count_Package;
      Total         : Natural := 0;
      aCount        : Natural;
      aString       : Unbounded_String;
      Prob          : Natural;
   begin
      Put_Line ("Counts size:" & Natural'Image (Natural (Counts.Length)));
      for index in Counts.First_Key .. Counts.Last_Key loop
         if Counts.Contains (index) then
            Total := Total + Counts.Element (index);
            --              Put_Line ("Total:" & Natural'Image (Total));
         end if;
      end loop;

      --        Put_Line ("Probabilities:");
      for index in Counts.First_Key .. Counts.Last_Key loop
         if Counts.Contains (index) then
            aCount := Counts.Element (index);
            --              Put_Line ("aCount:" & Natural'Image (aCount));
            Prob := (100 * aCount) / Total;
            aString :=
              To_Unbounded_String (Natural'Image (Prob) & "%");
            --              Probabilities.Replace (index, Prob);
            --              Put_Line (To_String (aString));
         end if;
      end loop;
      return To_String (aString);
   end Print_Leaf;

   --  --------------------------------------------------------------------------

   procedure Print_Question (Self : Raw_Question) is
      --  Example" Self = ("Colour", "Green"));
      --        use Feature_Map_Package;
      Col        : constant String := To_String (Self.Feature);
      Value      : constant String := To_String (Self.Value);
      --        Feature_ID : constant Class_Range := Features.Element (Col);
      --        V_Type     : constant Feature_Type := Find_Type (Value);
      --        QD         : Question_Data (V_Type);
      --        theKey : Unbounded_String := To_Unbounded_String ("Unknown");

      --        procedure Find_Key (Curs : Cursor) is
      --        begin
      --           if Features.Element (Curs) = Col then
      --              theKey := Key (Curs);
      --           end if;
      --        end Find_Key;
   begin
      --        Features.Iterate (Find_Key'Access);
      --        QD.Column := Self.Feature;
      --        case V_Type is
      --           when Integer_Type => QD.Integer_Value := Integer'Value (Value);
      --           when Float_Type => QD.Float_Value := Float'Value (Value);
      --           when Boolean_Type => QD.Boolean_Value := Boolean'Value (Value);
      --           when UB_String_Type => qd.UB_String_Value := To_Unbounded_String (Value);
      --        end case;
      Put_Line ("Is " & Col & " = " & " " & Value);
   end Print_Question;

   --  --------------------------------------------------------------------------

   procedure Print_Row (Label : String; Row : Row_Data) is
   begin
      Put (Label);
      Put (": (");
      for feat in Row.Features'First .. Row.Features'Last (1) loop
         Put (To_String (Row.Features (feat)));
      end loop;
      Put (") " & To_String (Row.Label));
      New_Line;
   end Print_Row;

   --  --------------------------------------------------------------------------

   procedure Print_Rows (Label : String; Rows : Rows_Vector) is
      use Rows_Package;
      aRow : Row_Data;
   begin
      Put_Line (Label);
      for index in Rows.First_Index .. Rows.Last_Index loop
         aRow := Rows.Element (index);
         Put ("(");
         for feat in aRow.Features'First .. aRow.Features'Last (1) loop
            Put (To_String (aRow.Features (feat)));
         end loop;
         Put (") " & To_String (aRow.Label));
         if index /= Rows.Last_Index then
            Put (", ");
         end if;
      end loop;
      New_Line;
   end Print_Rows;

   --  --------------------------------------------------------------------------

   procedure Print_Tree (aTree : Tree_Package.Tree) is
      use Tree_Package;
      procedure Print_Node (Curs : Cursor) is
         Node : constant Decision_Node_Type := Element (Curs);
      begin
         if Is_Leaf  (Curs) then
            Put_Line ("Leaf node");
         end if;
         Put ("Depth:" & Integer'Image (Integer (Depth (Curs))) & ", ");
         case Node.Node_Type is
            when  Decision_Kind =>
               --              Print_Question (Node.Question);
               Print_Rows ("True Rows", Node.True_Rows);
               Print_Rows ("False Rows", Node.False_Rows);
            when Prediction_Kind =>
               Put_Line ("Prediction; " & Natural'Image
                         (Node.Predictions.First_Element));
               New_Line;
         end case;
      end Print_Node;
   begin
      --  Iterate calls Print_Node.all with a cursor that designates each
      --  element in aTree starting with the root node and proceeding in a
      --  depth-first order.
      New_Line;
      Put_Line ("Depth first tree traversal");
      aTree.Iterate (Print_Node'Access);
   end Print_Tree;

   --  ---------------------------------------------------------------------------

   --     procedure Print_Unique_Values (Rows    : Rows_Vector;
   --                                    Feature : Feature_Type) is
   --        use Value_Set_Package;
   --        Values : constant Value_Set := Unique_Values (Rows, Feature);
   --        Curs   : Cursor := Values.First;
   --        Data   : Value_Data (Feature);
   --     begin
   --        Put ("Unique " & Feature_Type'Image (Feature)  & " Values:");
   --        while Has_Element (Curs) loop
   --           case Feature is
   --              when Colour_Feature =>
   --                 Data := Element (Curs);
   --                 Put (" " & Colour_Type'Image (Data.Colour));
   --              when Diameter_Feature  =>
   --                 Data := Element (Curs);
   --                 Put (Integer'Image (Data.Diameter) & " ");
   --           end case;
   --           Next (Curs);
   --        end loop;
   --        New_Line;
   --     end Print_Unique_Values;

   --  -----------------------------------------------------------------------

   procedure Set_Feature_Map (Features_Array : Feature_Names) is
   begin
      for index in Features_Array'Range loop
         --           Put_Line ("Set_Feature_Map feature: " &
         --                       To_String (Features_Array (index)));
         if not Features.Contains (Features_Array (index)) then
            Features.Insert (Features_Array (index), index);
            --              Put_Line ("Set_Feature_Map feature added.");
         end if;
         --           Put_Line ("Set_Feature_Map feature code: " &
         --                       Class_Range'Image (Features.Element (Features_Array (index))));
      end loop;
   end Set_Feature_Map;

   --  --------------------------------------------------------------------------

   --     procedure Set_Feature_ID (Feature : String; Feat_ID : Class_Range) is
   --        use Ada.Strings;
   --     begin
   --        if Fixed.Count (Feature, ".") = 1 then
   --           Features.Insert (To_Unbounded_String (Feature), Feat_ID);
   --        elsif Is_Integer (Feature) then
   --           Features.Insert (To_Unbounded_String (Feature), Feat_ID);
   --        elsif Is_Boolean (Feature) then
   --           Features.Insert (To_Unbounded_String (Feature), Feat_ID);
   --        else
   --           Features.Insert (To_Unbounded_String (Feature), Feat_ID);
   --        end if;
   --     end Set_Feature_ID;

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
      Value  : constant String := To_String (Q.Value);
      Q_Data : Question_Data;
   begin
      if Is_Integer (Q.Value) then
         declare
            QD  : Question_Data (Integer_Type);
         begin
            QD.Integer_Value := Integer'Value (Value);
            Q_Data :=  QD;
         end;
      elsif Is_Float (Q.Value) then
         declare
            QD  : Question_Data (Float_Type);
         begin
            QD.Float_Value := Float'Value (Value);
            Q_Data := QD;
         end;
      elsif Is_Boolean (Q.Value) then
         declare
            QD  : Question_Data (Boolean_Type);
         begin
            QD.Boolean_Value := Boolean'Value (Value);
            Q_Data := QD;
         end;
      else
         declare
            QD  : Question_Data (UB_String_Type);
         begin
            QD.UB_String_Value := To_Unbounded_String (Value);
            Q_Data := QD;
         end;

      end if;
      Q_Data.Feature := Q.Feature;
      return Q_Data;
   end To_Question;

   --  --------------------------------------------------------------------------

   --     function To_Row_Data (aRow : Unbounded_String) return Row_Data is
   --     begin
   --        return Parse (To_String (aRow));
   --     end To_Row_Data;

   --  --------------------------------------------------------------------------

   function To_Vector (Rows : Data_Rows; Header_Row : out Header_Data)
                       return Rows_Vector is
      New_Vector  : Rows_Vector;
      First_Index : constant Positive := Rows'First;
      aRow        : Row_Data;
   begin
      Header_Row := Parse_Header (To_String (Rows (Rows'First)));
      Set_Feature_Map (Header_Row.Features);
      for index in Positive'Succ (First_Index) .. Rows'Last loop
         aRow := Parse (To_String (Rows (index)));
         New_Vector.Append (aRow);
      end loop;
      return New_Vector;
   end To_Vector;

   --  --------------------------------------------------------------------------

   function UB_Class_Counts (Rows : Rows_Vector) return UB_Label_Map is
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
            --              Put_Line ("Label replaced "  &
            --                          Data_Type'Image (Label.Label_Kind) &  " " &
            --                         Integer'Image (Label_Counts.Element (Label.UB_String_Value)));
         end if;
      end loop;
      --        Put_Line ("Label_Counts size "  & Integer'Image (Integer (Label_Counts.Length)));
      return Label_Counts;
   end UB_Class_Counts;

   --  ---------------------------------------------------------------------------

   --     function Unique_Values (Rows    : Rows_Vector;
   --                             Feature : Feature_Type) return Value_Set is
   --        Data   : Row_Data;
   --        Value  : Value_Data (Feature);
   --        theSet : Value_Set;
   --     begin
   --
   --        for index in Rows.First_Index .. Rows.Last_Index loop
   --           Data := Rows.Element (index);
   --           if Feature = Colour_Feature then
   --              Value.Colour := Data.Colour;
   --           else
   --              Value.Diameter := Data.Diameter;
   --           end if;
   --           if not theSet.Contains (Value) then
   --              theSet.Append (Value);
   --           end if;
   --        end loop;
   --        return theSet;
   --     end Unique_Values;

   --  --------------------------------------------------------------------------

begin

   Label_Types.Insert (1, Integer_Type);
   Label_Types.Insert (2, Float_Type);
   Label_Types.Insert (3, Boolean_Type);
   Label_Types.Insert (4, UB_String_Type);

end Builder;
