
with Ada.Characters.Handling;
with Ada.Containers;
with Ada.Strings.Fixed;
with Ada.Text_IO;

package body Utilities is

   use ML_Types;

   procedure Print_Results_Question (Question : ML_Types.Question_Data);
   function Unique_Values (Rows    : Rows_Vector;
                           Feature : Feature_Name_Type) return Value_Set;

   --  --------------------------------------------------------------------------

   procedure Check_Rows (Rows : in out Rows_Vector) is
      use Ada.Containers;
      use Rows_Package;
      Data          : Row_Data := Rows.First_Element;
      Num_Features  : constant Class_Range := Data.Class_Count;
      Feature_Types : Data_Type_Array (1 .. Num_Features);
      Label_Type    : Data_Type := Get_Data_Type (Data.Label);
      Data_Changed  : Boolean := False;
   begin
      if Rows.Length < 2 then
         raise Utilities_Exception with
           "Utilities.Check_Rows called with empty rows vector";
      else
         for index in 1 .. Num_Features loop
            Feature_Types (index) := Get_Data_Type (Data.Features (index));
         end loop;

         for row in
           Integer'Succ (Rows.First_Index) .. Rows.Last_Index loop
            Data := Rows.Element (Integer (row));
            Data_Changed := False;
            --                  Print_Row ("Utilities.Check_Rows row", Data);
            for col in Class_Range range 1 .. Num_Features loop
               if Get_Data_Type (Data.Features (col)) = Float_Type and then
                 Feature_Types (col) = Integer_Type then
                  Data.Features (col) := Data.Features (col) & ".0";
                  Feature_Types (col) := Float_Type;
                  Data_Changed := True;
               elsif
                 Get_Data_Type (Data.Features (col)) = Integer_Type and then
                 Feature_Types (col) = Float_Type then
                  Data.Features (col) := Data.Features (col) & ".0";
                  Data_Changed := True;
               end if;
            end loop;

            if Get_Data_Type (Data.Label) = Float_Type and then
              Label_Type = Integer_Type then
               Data.Label := Data.Label & ".0";
               Label_Type := Float_Type;
               Data_Changed := True;
            end if;
            if Data_Changed then
               Rows.Replace_Element (row, Data);
            end if;
         end loop;
      end if;
   end Check_Rows;

   --  --------------------------------------------------------------------------

   function Get_Data_Type (Data : Unbounded_String) return ML_Types.Data_Type is
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

   function Is_Boolean (Item : in Unbounded_String) return Boolean is
      Item_String : constant String :=
                      Ada.Characters.Handling.To_Upper (To_String (Item));
   begin
      return Item_String = "TRUE" or else Item_String = "FALSE";
   end Is_Boolean;

   --  -------------------------------------------------------------------------

   function Is_Float (Item : in Unbounded_String) return Boolean is
      Item_String : constant String := To_String (Item);
      use Ada.Strings;
   begin
      return Fixed.Count (Item_String, ".") = 1;
   end Is_Float;

   --  -------------------------------------------------------------------------

   function Is_Integer (Item : in Unbounded_String) return Boolean is
      UB_String   : Unbounded_String := Item;
      Dig         : Boolean := True;
   begin
      UB_String := Trim (UB_String, Ada.Strings.Left);
      UB_String := Trim (UB_String, Ada.Strings.Right);
      declare
         Item_String : constant String := To_String (UB_String);
      begin
         for index in Item_String'Range loop
            Dig := Dig and then
              Ada.Characters.Handling.Is_Decimal_Digit (Item_String (index));
         end loop;
      end;
      return Dig;
   end Is_Integer;

   --  ---------------------------------------------------------------------------

   function Label_Array (Data : ML_Types.Rows_Vector) return Label_Data_Array is
      Data_Array : Label_Data_Array (Data.First_Index .. Data.Last_Index);
      UB_Label   : Unbounded_String;
      Data_Kind  : Data_Type;
   begin
      for index in Data.First_Index .. Data.Last_Index loop
         UB_Label := Data.Element (index).Label;
         Data_Kind := Get_Data_Type (UB_Label);
         declare
            Label   : Value_Record (Data_Kind);
            Label_S : constant String := To_String (Data.Element (index).Label);
         begin
            case Label.Value_Kind is
               when Boolean_Type =>
                  Label.Boolean_Value := Boolean'Value (Label_S);
               when Float_Type =>
                  Label.Float_Value := Float'Value (Label_S);
               when Integer_Type =>
                  Label.Integer_Value := Integer'Value (Label_S);
               when UB_String_Type =>
                  Label.UB_String_Value := Data.Element (index).Label;
            end case;
            Data_Array (index) := Label;
         end; --  declare block
      end loop;
      return Data_Array;
   end Label_Array;

   --  ---------------------------------------------------------------------------

   procedure Load_CSV_Data (Data_File : File_Type;
                            Data      : out ML_Types.Rows_Vector) is
      use Ada.Strings.Unbounded;
      use ML_Types;
      use ML_Types.String_Package;
      Data_Line    : Unbounded_String :=
                       To_Unbounded_String (Get_Line (Data_File));
      Num_Features : ML_Types.Class_Range;
      CSV_Line     : String_List;
      Curs         : ML_Types.String_Package.Cursor;
   begin
      Num_Features :=
        Class_Range (Ada.Strings.Fixed.Count (To_String (Data_Line), ","));
      Builder.Set_Header_Data (To_String (Data_Line));

      declare
         Values       : Feature_Data_Array (1 .. Num_Features);
      begin
         while not End_Of_File (Data_File) loop
            declare
               Value_Index  : Class_Range := 1;
               Row          : Row_Data (Num_Features);
            begin
               Data_Line := To_Unbounded_String (Get_Line (Data_File));
               CSV_Line := Utilities.Split_String
                 (To_String (Data_Line), ",");
               Curs := CSV_Line.First;
               while Has_Element (Curs) loop
                  if Curs /= CSV_Line.Last then
                     Values (Value_Index) := Element (Curs);
                     Value_Index := Value_Index + 1;
                  else
                     Row.Label := Element (Curs);
                  end if;
                  Next (Curs);
               end loop;
               Row.Features := Values;
               Data.Append (Row);
            end;  --  declare block
         end loop;
      end;  --  declare block
      --        Put_Line ("Data length: " & Count_Type'Image (Data.Data.Length));
      --        Print_Data_Item (Data.Data, Num_Features, 15);
      --        Print_Data (Data.Data, Num_Features);

   end Load_CSV_Data;

   --  -------------------------------------------------------------------------

   function Number_Of_Features (Rows : Rows_Vector) return Class_Range is
      Data  : constant Row_Data := Rows.First_Element;
   begin
      return Data.Class_Count;
   end Number_Of_Features;

   --  -------------------------------------------------------------------------

   function Number_Of_Features (Rows : Value_Data_List) return Class_Range is
   begin
      return Class_Range (Rows.Length);
   end Number_Of_Features;

   --  -------------------------------------------------------------------------

   function Predictions (Node : Tree_Node_Type) return Predictions_List is
      use ML_Types;
      use Prediction_Data_Package;
      Num_Rows        : constant Positive := Positive (Node.Rows.Length);
      Curs            : Cursor;
      Label           : Unbounded_String;
      Data            : Prediction_Data;
      thePredictions  : Predictions_List;
      Found           : Boolean := False;
   begin
      for index in 1 .. Num_Rows loop
         Label := Node.Rows.Element (index).Label;
         Curs := thePredictions.First;
         Found := False;
         while Has_Element (Curs) and then not Found loop
            Data := Element (Curs);
            Found := Element (Curs).Label = Label;
            if Found then
               Data.Num_Copies := Data.Num_Copies + 1;
               thePredictions.Replace_Element (Curs, Data);
            end if;
            Next (Curs);
         end loop;

         if not Found then
            Data.Label := Label;
            thePredictions.Append (Data);
         end if;
      end loop;
      return thePredictions;

   end Predictions;

   --  ------------------------------------------------------------------------

   procedure Print_Best (Message : String; Best_Split : Builder.Best_Data) is
      Question     : constant Question_Data :=
                       Builder.Best_Question (Best_Split);
      Feature      : constant String := To_String (Question.Feature_Name);
      Feature_Kind : constant Data_Type := Question.Feature_Kind;
   begin
      New_Line;
      Put_Line (Message & " best question:");
      Put (Feature & " = ");
      case Feature_Kind is
         when Integer_Type =>
            Put_Line (Integer'Image (Question.Integer_Value));
         when Float_Type =>
            Put_Line (Float'Image (Question.Float_Value));
         when Boolean_Type =>
            Put_Line (Boolean'Image (Question.Boolean_Value));
         when UB_String_Type => Put_Line (To_String (Question.UB_String_Value));
      end case;
      Put_Line ("Gain = " & Float'Image (Builder.Gain (Best_Split)));

   end Print_Best;

   --  ------------------------------------------------------------------------

   procedure Print_Classification (Classification : Predictions_List) is
      use Prediction_Data_Package;
      Curs        : Cursor := Classification.First;
      Data        : Prediction_Data;
      Predictions : Unbounded_String;
   begin
      Put ("Classification:  {");
      while Has_Element (Curs) loop
         Data := Element (Curs);
         Predictions := Predictions & "'" & To_String (Data.Label) &
           "':" & Natural'Image (Data.Num_Copies);
         if not (Curs = Classification.Last) then
            Predictions := Predictions & ", ";
         end if;
         Next (Curs);
      end loop;
      Predictions := Predictions & "}";
      Put_Line (To_String (Predictions));

   exception
      when others =>
         Put_Line ("Print_Classification exception");
         raise;
   end Print_Classification;

   --  --------------------------------------------------------------------------

   procedure Print_Leaf (Label_Counts : Predictions_List) is
      use Prediction_Data_Package;
      Count_Cursor : Cursor := Label_Counts.First;
      Prediction   : Prediction_Data;
      Total        : Natural := 0;
   begin
      Put_Line ("Predictions:");
      while Has_Element (Count_Cursor) loop
         Total := Total + Element (Count_Cursor).Num_Copies;
         Next (Count_Cursor);
      end loop;

      Count_Cursor := Label_Counts.First;
      while Has_Element (Count_Cursor) loop
         Prediction := Element (Count_Cursor);
         Put_Line  ("{'" & To_String (Prediction.Label) & "': '" &
                      Integer'Image ((100 * Prediction.Num_Copies) / Total) &
                      "%'}");
         Next (Count_Cursor);
      end loop;
   end Print_Leaf;

   --  -------------------------------------------------------------------------

   procedure Print_Value_Record (Message : String; Value : Value_Record) is
      Value_Kind : constant Data_Type := Value.Value_Kind;
   begin
      New_Line;
      Put_Line (Message & " value record:");
      case Value_Kind is
         when Integer_Type =>
            Put_Line (Integer'Image (Value.Integer_Value));
         when Float_Type =>
            Put_Line (Float'Image (Value.Float_Value));
         when Boolean_Type =>
            Put_Line (Boolean'Image (Value.Boolean_Value));
         when UB_String_Type => Put_Line (To_String (Value.UB_String_Value));
      end case;

   end Print_Value_Record;

   --  ------------------------------------------------------------------------

   function Prediction_String (Label_Counts : Predictions_List)
                               return String is
      use Prediction_Data_Package;
      Count_Cursor : Cursor := Label_Counts.First;
      Prediction   : Prediction_Data;
      Total        : Natural := 0;
      Leaf_Data    : Unbounded_String := To_Unbounded_String
        ("{'");
   begin
      while Has_Element (Count_Cursor) loop
         Total := Total + Element (Count_Cursor).Num_Copies;
         Next (Count_Cursor);
      end loop;
      Count_Cursor := Label_Counts.First;
      while Has_Element (Count_Cursor) loop
         Prediction := Element (Count_Cursor);
         Leaf_Data := Leaf_Data & To_Unbounded_String
           (To_String (Prediction.Label) & "': '" &
              Integer'Image ((100 * Prediction.Num_Copies) / Total) &
              "%'");
         if Count_Cursor /= Label_Counts.Last then
            Leaf_Data := Leaf_Data & ", ";
         end if;
         Next (Count_Cursor);
      end loop;
      return To_String (Leaf_Data) & "}";
   end Prediction_String;

   --  -------------------------------------------------------------------------

   procedure Print_Feature_Values (Message : String; Rows : Rows_Vector;
                                   Column  : Class_Range) is
      use Rows_Package;
      aRow : Row_Data;
   begin
      Put_Line (Message & ":");
      for index in Rows.First_Index .. Rows.Last_Index loop
         aRow := Rows.Element (index);
         Put ("  Feature value: ");
         Put (To_String (aRow.Features (Column)));
         if Column /= aRow.Features'Last then
            Put (", ");
         end if;
         New_Line;
      end loop;

   end Print_Feature_Values;

   --  ------------------------------------------------------------------------

   procedure Print_Node (Node : Tree_Node_Type) is

   begin
      Put_Line ("  Node data:");
      Put_Line ("    Node type " &  Node_Kind'Image (Node.Node_Type));
      Print_Question ("    Question", Node.Question);
      case Node.Node_Type is
         when Prediction_Kind =>
            Print_Rows ("        Rows:", Node.Rows);
            Print_Row ("    Prediction:", Node.Prediction);
         when Decision_Kind =>
            Print_Rows ("    True Rows:", Node.True_Branch);
            Print_Rows ("    False Rows:", Node.False_Branch);
      end case;

   end Print_Node;

   --  -------------------------------------------------------------------------

   procedure Print_Prediction (Node : Tree_Node_Type; Offset : String) is
      use ML_Types;
      use Prediction_Data_Package;
      Curs             : Cursor;
      Data             : Prediction_Data;
      Prediction_List  : constant Predictions_List := Node.Prediction_List;
      Prediction       : Unbounded_String;
   begin
      Put_Line (Offset & "    gini = " & Float'Image (Node.Gini));
      Prediction := To_Unbounded_String (Offset  & "    Predict {");
      Curs := Prediction_List.First;
      while Has_Element (Curs) loop
         Data := Element (Curs);
         Prediction := Prediction & "'" & To_String (Data.Label) &
           "':" & Natural'Image (Data.Num_Copies);
         if not (Curs = Prediction_List.Last) then
            Prediction := Prediction & ", ";
         end if;
         Next (Curs);
      end loop;
      Prediction := Prediction & "}";
      Put_Line (To_String (Prediction));

   end Print_Prediction;

   --  ------------------------------------------------------------------------

   procedure Print_Question (Message  : String;
                             Question : ML_Types.Question_Data) is
      Col          : constant String := To_String (Question.Feature_Name);
      Feature_Kind : constant Data_Type := Question.Feature_Kind;
   begin
      Put_Line (Message & " question:");
      Put ("  Feature " & "'" & Col & "'" & " = ");
      case Feature_Kind is
         when Integer_Type =>
            Put_Line (Integer'Image (Question.Integer_Value));
         when Float_Type =>
            Put_Line (Float'Image (Question.Float_Value));
         when Boolean_Type =>
            Put_Line (Boolean'Image (Question.Boolean_Value));
         when UB_String_Type => Put_Line (To_String (Question.UB_String_Value));
      end case;
      Put_Line ("  Gain " & Float'Image (Question.Gain));

   end Print_Question;

   --  --------------------------------------------------------------------------

   procedure Print_Raw_Question (Message : String; Question : Raw_Question) is
      --  Example" Self = ("Colour", "Green"));
      Col       : constant String := To_String (Question.Feature_Name);
      Value     : constant String := To_String (Question.Feature_Value);
      Data_Kind : constant Data_Type := Get_Data_Type (Question.Feature_Value);
   begin
      Put (Message);
      Put (" raw question: Is " & Col);
      case Data_Kind is
         when Integer_Type | Float_Type => Put (" >= ");
         when others => Put (" = ");
      end case;
      Put_Line (" " & Value);
   end Print_Raw_Question;

   --  ------------------------------------------------------------------------

   procedure Print_Results_Question (Question : ML_Types.Question_Data) is
      UB_String : Unbounded_String;
   begin
      Put ("Is " & To_String (Question.Feature_Name));
      case Question.Feature_Kind is
         when Integer_Type =>
            Put (" >= " & Integer'Image
                 (Question.Integer_Value));
         when Float_Type =>
            Put (" >= " & Float'Image
                 (Question.Float_Value));
         when Boolean_Type =>
            Put (" = " & Boolean'Image
                 (Question.Boolean_Value));
         when UB_String_Type =>
            UB_String := Question.UB_String_Value;
            if Is_Integer (UB_String) or else
              Is_Float (UB_String) then
               Put (" >= " & To_String (UB_String));
            else
               Put (" = " & To_String (UB_String));
            end if;
      end case;
      Put_Line ("?");

   end Print_Results_Question;

   --  ------------------------------------------------------------------------

   procedure Print_Row (Message : String; aRow : Row_Data) is
   begin
      Put_Line (Message);
      Put ("  Feature values: ");
      for feat in aRow.Features'First .. aRow.Features'Last loop
         Put (To_String (aRow.Features (feat)));
         if feat /= aRow.Features'Last then
            Put (", ");
         end if;
      end loop;
      Put_Line ("; Label: " & To_String (aRow.Label));
   end Print_Row;

   --  ------------------------------------------------------------------------

   procedure Print_Rows (Message : String; Rows : Rows_Vector) is
      use Rows_Package;
      aRow : Row_Data;
   begin
      Put_Line (Message & ":");
      for index in Rows.First_Index .. Rows.Last_Index loop
         aRow := Rows.Element (index);
         Put ("  Feature values: (");
         for feat in aRow.Features'First .. aRow.Features'Last loop
            Put (To_String (aRow.Features (feat)));
            if feat /= aRow.Features'Last then
               Put (", ");
            end if;
         end loop;
         Put_Line ("), Label: " & To_String (aRow.Label));
      end loop;

   end Print_Rows;

   --  ------------------------------------------------------------------------

   procedure Print_Tree (aTree : Tree_Type) is
      use Tree_Package;
      This_Indent : Natural := 0;
      Last_Offset : Unbounded_String;

      procedure Print_Tree_Node (Curs : Cursor; Indent : Natural := 0) is
         use Ada.Containers;
         Node         : Tree_Node_Type;
         True_Child   : Cursor;
         False_Child  : Cursor;
         Gini_Printed : Boolean := False;
         begin
            This_Indent := Indent + 1;
            if This_Indent > 10 then
               This_Indent := 1;
            end if;
            Node := Element (Curs);
            if Is_Leaf  (Curs) then
               Print_Prediction (Node, To_String (Last_Offset));
            else
               declare
                  Offset    : String (1 .. This_Indent + 1) := (others => ' ');
                  pos       : Natural := 1;
               begin
                  while pos < This_Indent - 1 loop
                     Offset (pos .. pos + 2) := "   ";
                     pos := pos + 2;
                  end loop;
                  if This_Indent > 1 and then pos < This_Indent + 1 then
                     Offset (Indent) := ' ';
                  end if;
                  Put (Offset);
                  Last_Offset := To_Unbounded_String (Offset);

                  if Node.Node_Type = Prediction_Kind then
                     Put_Line ("Print_Tree_Node non-leaf prediction encountered! ");
                     Print_Prediction (Node, Offset);
                  else
                     Print_Results_Question (Node.Question);
                     Put_Line (Offset & "--> True:");
                     if not Gini_Printed then
                     Put_Line (Offset & "    gini = " &
                                 Float'Image (Node.Gini));
                        Gini_Printed := True;
                     end if;
                     True_Child := First_Child (Curs);
                     Print_Tree_Node (True_Child, This_Indent + 1);

                     if Child_Count (Curs) > 1 then
                        False_Child := Next_Sibling (True_Child);
                        Put_Line (Offset & "--> False:");
                        if not Gini_Printed then
                        Put_Line (Offset & "    gini = " &
                                    Float'Image (Node.Gini));
                        Gini_Printed := True;
                        end if;
                        Print_Tree_Node (False_Child, This_Indent + 1);
                     end if;
                  end if;
               end; --  declare block
            end if;
         end Print_Tree_Node;

      begin
         Print_Tree_Node (First_Child (aTree.Root));
      end Print_Tree;

      --  -------------------------------------------------------------------------

      procedure Print_UB_Label_Counts (Rows : Rows_Vector) is
         use UB_Label_Map_Package;
         Label_Counts : constant UB_Label_Map :=
                          Builder.UB_Label_Counts (Rows);
         Count_Cursor : UB_Label_Map_Package.Cursor := Label_Counts.First;
         aCount       : Natural;
      begin
         Put_Line ("Label Counts:");
         while Has_Element (Count_Cursor) loop
            aCount := Element (Count_Cursor);
            Put_Line (To_String ((Key (Count_Cursor))) &  ": " &
                        Natural'Image (aCount));
            next (Count_Cursor);
         end loop;
      end Print_UB_Label_Counts;

      --  -------------------------------------------------------------------------

      procedure Print_Unique_Values (Rows    : Rows_Vector;
                                     Feature : Feature_Name_Type) is
         use Values_Package;
         Values : constant Value_Set := Unique_Values (Rows, Feature);
         Curs   : Cursor := Values.First;
         Data   : Value_Data;
      begin
         Put (To_String (Feature) & " Values:");
         while Has_Element (Curs) loop
            Data := Element (Curs);
            case Data.Feature_Kind is
            when Boolean_Type =>
               Put (" " & Boolean'Image (Data.Boolean_Value));
            when Float_Type =>
               Put (" " & Float'Image (Data.Float_Value));
            when Integer_Type =>
               Put (" " & Integer'Image (Data.Integer_Value));
            when UB_String_Type =>
               Put (" " & To_String (Data.UB_String_Value));
            end case;
            Next (Curs);
         end loop;
         New_Line;
      end Print_Unique_Values;

      --  -----------------------------------------------------------------------

      function Split_Row_Data (Row_Data : ML_Types.Rows_Vector) return Data_Record is
         use Rows_Package;
         use Value_Data_Package;
         aRow           : ML_Types.Row_Data := Row_Data.First_Element;
         Feature_Values : Value_Data_List;
         Features_List  : Features_Data_List;
         Feature_Types  : array  (1 .. aRow.Class_Count) of Data_Type;
         Label_Type     : Data_Type;
         Label_Values   : Value_Data_List;
         Data           : Data_Record;
      begin
         for index in aRow.Features'Range loop
            Data.Feature_Names.Append (aRow.Features (index));
         end loop;
         Data.Label_Name := aRow.Label;

         for row_index in Row_Data.First_Index + 1 .. Row_Data.Last_Index loop
            aRow := Row_Data.Element (row_index);
            declare
               Features                : constant Feature_Data_Array
                 (1 .. aRow.Class_Count) := aRow.Features;
               Label                   : constant Unbounded_String := aRow.Label;
            begin
               if row_index = Row_Data.First_Index + 1 then
                  Label_Type := Get_Data_Type (aRow.Label);
               end if;
               for f_index in Features'Range loop
                  if row_index = Row_Data.First_Index + 1 then
                     Feature_Types (f_index) :=
                       Get_Data_Type (aRow.Features (f_index));
                  end if;

                  declare
                     Feat_String : constant Unbounded_String := Features (f_index);
                     Value       : Value_Record (Feature_Types (f_index));
                  begin
                     case Feature_Types (f_index) is
                     when Boolean_Type =>
                        Value.Boolean_Value :=
                          Boolean'Value (To_String (Feat_String));
                     when Integer_Type =>
                        Value.Integer_Value :=
                          Integer'Value (To_String (Feat_String));
                     when Float_Type =>
                        Value.Float_Value :=
                          Float'Value (To_String (Feat_String));
                     when UB_String_Type =>
                        Value.UB_String_Value := Feat_String;
                     end case;
                     Feature_Values.Append (Value);
                  end;  --  declare block
               end loop;
               Features_List.Append (Feature_Values);

               declare
                  Label_Value    : Value_Record (Label_Type);
               begin
                  case Label_Type is
                  when Boolean_Type =>
                     Label_Value.Boolean_Value :=
                       Boolean'Value (To_String (Label));
                  when Integer_Type =>
                     Label_Value.Integer_Value :=
                       Integer'Value (To_String (Label));
                  when Float_Type =>
                     Label_Value.Float_Value :=
                       Float'Value (To_String (Label));
                  when UB_String_Type =>
                     Label_Value.UB_String_Value := Label;
                  end case;
                  Label_Values.Append (Label_Value);
               end;  --  declare block;
            end;
         end loop;

         Data.Feature_Values := Features_List;
         Data.Label_Values := Label_Values;
         return Data;

      end Split_Row_Data;

      --  -----------------------------------------------------------------------

      function Split_String (aString, Pattern : String) return String_List is
         use Ada.Strings;
         use Ada.Strings.Unbounded;
         Last       : constant Integer := aString'Length;
         A_Index    : Integer;
         B_Index    : Integer := 1;
         Split_List : String_List;
      begin
         for index in 1 .. Fixed.Count (aString, Pattern) loop
            A_Index := Fixed.Index (aString (B_Index .. Last), Pattern);
            --  process string slice in any way
            Split_List.Append (To_Unbounded_String (aString (B_Index .. A_Index - 1)));
            B_Index := A_Index + Pattern'Length;
         end loop;
         --  process last string
         Split_List.Append (To_Unbounded_String (aString (B_Index .. Last)));
         return Split_List;

      end Split_String;

      --  -------------------------------------------------------------------------

      function Unique_Values (Rows    : Rows_Vector;
                              Feature : Feature_Name_Type) return Value_Set is
         use Ada.Containers;
         use Rows_Package;
         use Values_Package;
         Data              : Row_Data := Rows.First_Element;
         Row2              : constant Row_Data :=
                               Rows.Element (Positive'Succ (Rows.First_Index));
         Num_Features      : constant Class_Range := Data.Class_Count;
         Row2_Features     : constant Feature_Data_Array (1 .. Num_Features) :=
                               Row2.Features;
         Feature_Name      : Feature_Name_Type;
         Feature_Data_Type : Data_Type;
         Value_String      : Unbounded_String;
         theSet            : Value_Set;

         procedure Add_To_Set (Value : Value_Data) is
         begin
            if not theSet.Contains (Value) then
               theSet.Append (Value);
            end if;
         end Add_To_Set;

      begin
         if Rows.Length < 2 then
            raise Utilities_Exception with
              "Utilities.Unique_Values called with empty rows vector";
         else
            for index in Class_Range range
              Class_Range'Succ (Class_Range (Rows.First_Index)) ..
                Class_Range (Rows.Last_Index) loop
               Data := Rows.Element (Integer (index));
               for col in Class_Range range
                 1 .. Num_Features loop
                  Feature_Name :=
                    Feature_Name_Type (Rows.First_Element.Features (col));
                  if Feature_Name = Feature then
                     Feature_Data_Type :=
                       Get_Data_Type (Row2_Features (col));
                     Value_String := Data.Features (col);
                     case Feature_Data_Type is
                     when Boolean_Type =>
                        declare
                           Feature_Value : Value_Data (Boolean_Type);
                        begin
                           Feature_Value.Feature_Name := Feature;
                           Feature_Value.Boolean_Value :=
                             Boolean'Value (To_String (Value_String));
                           Add_To_Set (Feature_Value);
                        end;

                     when Float_Type =>
                        declare
                           Feature_Value : Value_Data (Float_Type);
                        begin
                           Feature_Value.Feature_Name := Feature;
                           Feature_Value.Float_Value :=
                             Float'Value (To_String (Value_String));
                           Add_To_Set (Feature_Value);
                        end;

                     when Integer_Type =>
                        declare
                           Feature_Value : Value_Data (Integer_Type);
                        begin
                           Feature_Value.Feature_Name := Feature;
                           Feature_Value.Integer_Value :=
                             Integer'Value (To_String (Value_String));
                           Add_To_Set (Feature_Value);
                        end;

                     when UB_String_Type =>
                        declare
                           Feature_Value : Value_Data (UB_String_Type);
                        begin
                           Feature_Value.Feature_Name := Feature;
                           Feature_Value.UB_String_Value := Value_String;
                           Add_To_Set (Feature_Value);
                        end;
                     end case;
                  end if;
               end loop;
            end loop;
         end if;
         return theSet;
      end Unique_Values;

      --  --------------------------------------------------------------------------

   end Utilities;
