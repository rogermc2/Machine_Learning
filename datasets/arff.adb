--  Based on scikit-learn/sklearn/externals _arff.py

with Ada.Assertions; use Ada.Assertions;
with Ada.Containers.Doubly_Linked_Lists;
with Ada.Containers.Ordered_Maps;
with Ada.Integer_Text_IO;
with Ada.Calendar;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
--  with Ada.Strings.Unbounded.Text_IO; use Ada.Strings.Unbounded.Text_IO;
with Ada.Text_IO; use Ada.Text_IO;

with GNAT.Regpat;

with Dataset_Utilities; use Dataset_Utilities;
with ML_Types; use ML_Types;
--  with Printing;
with Regexep;

--  pragma Warnings (Off);

package body ARFF is

   type TK_State is (TK_Descrition, TK_Relation, TK_Attribute, TK_Data);
   --                       TK_Comment);
   --      type Conversor_Type is (Conversor_Unencoded, Conversor_Encoded,
   --                              Conversor_Map);
   type Conversor_Data_Type is (Conv_Integer, Conv_Numeric, Conv_Real,
                                Conv_String, Conv_Nominal);

   --      type Conversor_Data (Encoded : Conversor_Type := Conversor_Map)
   --      is record
   --          case Encoded is
   --              when Conversor_Unencoded =>
   --                  Values         : ML_Types.String_List;
   --              when Conversor_Encoded =>
   --                  Encoded_Values : ML_Types.String_List;
   --                  Zero_Value     : Unbounded_String := To_Unbounded_String ("");
   --              when Conversor_Map =>
   --                  Mapping        : Unbounded_String := To_Unbounded_String ("");
   --          end case;
   --      end record;

   --     type Converser_Map is record
   --        Text    : Unbounded_String := To_Unbounded_String ("");
   --        Int     : Integer := 0;
   --        Numeric : Float := 0.0;
   --        Real    : Float := 0.0;
   --     end record;

   --      package Conversor_Package is new
   --        Ada.Containers.Doubly_Linked_Lists (Conversor_Data);
   --      subtype Conversor_List is Conversor_Package.List;

   --     type Conversor_Tuple is record
   --        Key   : Conversor_Data;
   --        Value : Unbounded_String;
   --     end record;
   --
   --     package Conversor_Tuple_Package is new
   --       Ada.Containers.Doubly_Linked_Lists (Conversor_Tuple);
   --     subtype Conversor_Tuple_List is Conversor_Tuple_Package.List;

   type Conversor_Item is record
      Name         : Unbounded_String;
      Data_Type    : Conversor_Data_Type;
      Nominal_List : Indef_String_List;
   end record;

   package Conversor_Item_Package is new
     Ada.Containers.Doubly_Linked_Lists (Conversor_Item);
   subtype Conversor_Item_List is Conversor_Item_Package.List;

   type Arff_Decoder is record
      Conversers   : Conversor_Item_List;
      Current_Line : Integer := 0;
   end record;

   package Escape_Sub_Map_Package is new
     Ada.Containers.Ordered_Maps (Unbounded_String, Unbounded_String);

   --     type Stream_Func_Type is access function (Decoder : in out Arff_Decoder)
   --                                               return String;

   Escape_Sub_Map : Escape_Sub_Map_Package.Map;
   --     Stream_Cursor  : ML_Types.String_Package.Cursor;
   --  L190
   Quoted_Re      : constant String :=
                      "''""(?:(?:\\\\)*\\""|\\|[^""]|[^""\\])*""";
   --                        "''""(?:(?<!\\)(?:\\\\)*\\""|\\|[^""]|[^""\\])*""";
   Quoted_Re2     : constant String :=
                      "'''(?:(?:\\\\)*\\'|\\[^']|[^'\\])*''";
   --                        "'''(?:(?<!\\)(?:\\\\)*\\'|\\[^']|[^'\\])*''";
   --  L204
   Value_Re       : constant String := "''(?:''" & Quoted_Re & "|" &
                      Quoted_Re2 & "|[^,\s""'{}]+)''";
   Stream_Cursor  : String_Package.Cursor;

   procedure Decode_Attribute (Decoder        : in out Arff_Decoder; UC_Row : String;
                               Arff_Container : in out JSON_Value);
   procedure Decode_Comment
     (UC_Row : String; Arff_Container : in out JSON_Value);
   procedure Decode_Relation
     (UC_Row : String; Arff_Container : in out JSON_Value);
   --     function Decode_COO_Rows
   --       (Decoder : in Out Arff_Decoder; Stream_Func : Stream_Func_Type;
   --        Data    : out Classifier_Types.Float_List; Rows, Cols : out Integer_List)
   --        return String_List;
   function Decode_Dense_Rows (Decoder       : in Out Arff_Decoder;
                               Stream        : String_List)
                               return JSON_Array;
   function Decode_Dense_Values
     (Values : Indef_String_List; Attribute_List : Conversor_Item_List)
      return JSON_Array;
   function Parse_Values (Row : String) return Indef_String_List;
   function Split_Sparse_Line (Row : String) return Indef_String_List;
   function Unquote (Values : String) return String;
   function Unquote (Values : String) return Unbounded_String;

   --  -------------------------------------------------------------------------

   --  Build_Re_Dense and Build_Re_Sparse (_RE_DENSE_VALUES) tokenize
   --  despite quoting, whitespace, etc.
   --  Build_Re_Dense captures (value, error) groups.
   --  Because empty values are allowed we cannot just look for empty values
   --  to handle syntax errors.
   --  the line is assumed to have ',' prepended.
   --  L214
   function Build_Re_Dense return GNAT.Regpat.Pattern_Matcher is
      use GNAT.Regpat;
      --        Regex  : constant String := "''(?x),\s*((?=,)|$|{" &
      Regex  : constant String := ",\s*(|$|{" &
                 Value_Re & "})|(\S.*)";
   begin
      declare
         Result : constant GNAT.Regpat.Pattern_Matcher :=  Compile (Regex);
      begin
         return Result;
      end;

   end Build_Re_Dense;

   --  -------------------------------------------------------------------------
   --  L225
   function Build_Re_Sparse return GNAT.Regpat.Pattern_Matcher is
      use GNAT.Regpat;
      --        Regex : constant String :=  "(?x)(?:^\s*\{|,)\s*(\d+)\s+("
      Regex : constant String := "(?:^\s*\{|,)\s*(\d+)\s+("
                & Value_Re & "s)|\S.*";
      --                  & Value_Re & "s)|(?!}\s*$)(?!^\s*{\s*}\s*$)\S.*";
   begin
      declare
         Result : constant GNAT.Regpat.Pattern_Matcher :=  Compile (Regex);
      begin
         --           Put_Line ("ARFF.Build_Re_Sparse Regex compiled");
         return Result;
      end;

   end Build_Re_Sparse;

   --  -------------------------------------------------------------------------
   --  L771  and, by reference, L879
   function Decode_ARFF (Decoder        : in Out Arff_Decoder;
                         Text           : ML_Types.String_List;
                         --                           Encode_Nominal : Boolean := False;
                         Matrix_Type    : ARFF_Return_Type := Arff_Dense)
                         return JSON_Value is
      use ML_Types.String_Package;
      Routine_Name    : constant String := "ARFF.Decode_ARFF ";
      Bad_Layout      : constant String := " layout of ARFF file is bad.";
      --        Text_Length     : constant Integer := Text'Length;
      --        Text_Length     : constant Positive := Positive (Length (Text));
      State           : TK_State := TK_Descrition;
      --        Pos1            : Integer := 1;
      --        Pos2            : Integer := 1;
      --        Message_Lines   : String_List;
      Curs            : Cursor := Text.First;
      Values          : JSON_Array;
      Arff_Container  : JSON_Value := Create_Object;
   begin
      Decoder.Current_Line := 0;
      Put_Line (Routine_Name);

      --        while Pos2 /= 0 and Pos1 < Text_Length loop
      --           Pos2 := Fixed.Index (Text (Pos1 .. Text_Length), "\r\n");
      --           Message_Lines.Append
      --             (To_Unbounded_String
      --                (Fixed.Trim (Text (Pos1 .. Pos2 - 1), Both)));
      --           Pos1 := Pos2 + 4;
      --        end loop;

      --        Put_Line (Routine_Name & "Message_Lines:");
      --        Curs := Message_Lines.First;
      --        while Has_Element (Curs) loop
      --           Put_Line (To_String (Element (Curs)));
      --           Next  (Curs);
      --        end loop;
      --        Put_Line (Routine_Name & "end of Message_Lines");
      --        New_Line;

      --        if Pos1 < Text_Length - 4 then
      --           Message_Lines.Append
      --             (To_Unbounded_String (Text (Pos1 .. Text_Length - 4)));
      --        end if;

      --  L784 Arff_Container implements obj: ArffContainerType
      Arff_Container.Set_Field ("description", Create (Empty_Array));
      Arff_Container.Set_Field ("relation", "");
      Arff_Container.Set_Field ("attributes", Create (Empty_Array));
      Arff_Container.Set_Field ("data", Create (Empty_Array));

      --  L796  Read all lines
      Curs := Text.First;
      while Has_Element (Curs) and State /= TK_Data loop
         Decoder.Current_Line := Decoder.Current_Line + 1;
         declare
            UC_Row : constant String := Dataset_Utilities.To_Upper_Case
              (To_String (Element (Curs)));
         begin
            if UC_Row /= "" then
               if UC_Row'Length >= 1 and then UC_Row (1 .. 1) = "%"
               then
                  Assert (State = TK_Descrition, Routine_Name & Bad_Layout &
                            "TK_Descrition state expected but in " &
                            TK_State'Image (State) & " state");
                  Decode_Comment (UC_Row, Arff_Container);

               elsif UC_Row'Length >= 9 and then UC_Row (1 .. 9) = "@RELATION"
               then
                  Assert (State = TK_Descrition, Routine_Name & Bad_Layout &
                            "TK_Descrition state expected but in " &
                            TK_State'Image (State) & " state");
                  State := TK_Relation;
                  Decode_Relation (UC_Row, Arff_Container);

                  --  L821 _TK_ATTRIBUTE = "@ATTRIBUTE"
               elsif UC_Row'Length >= 10 and then
                 UC_Row (1 .. 10) = "@ATTRIBUTE"
               then
                  Assert (State = TK_Relation or State = TK_Attribute,
                          Routine_Name & Bad_Layout &
                            "TK_Relation or TK_Attribute state expected but in "
                          & TK_State'Image (State) & " state");
                  State := TK_Attribute;
                  --                    Put_Line (Routine_Name & "Attribute state, UC_Row: " & UC_Row);
                  Decode_Attribute (Decoder, UC_Row, Arff_Container);
                  --  _decode lines 827 - 846 (update attribute_names)
                  --  are implemented in Decode_Attribute

               elsif UC_Row'Length >= 5 and then UC_Row (1 .. 5) = "@DATA" then
                  --  L850
                  Assert (State = TK_Attribute, Routine_Name &
                            "TK_Attribute state expected but in " &
                            TK_State'Image (State) & " state");
                  State := TK_Data;

               elsif UC_Row (1 .. 1) = "%" then
                  --  L806
                  Decode_Comment (UC_Row, Arff_Container);

               else
                  --                    Put_Line (Routine_Name & "Row: " & UC_Row);
                  Assert (False, Routine_Name & "in unexpected state " &
                            TK_State'Image (State));
               end if;
            end if;
         end;

         Next (Curs);
      end loop;
      Put_Line (Routine_Name & "L872");

      Stream_Cursor := Curs;
      --  L872 Alter the data object
      --  case Matrix_Type implements
      --  L792 data = _get_data_object_for_decoding(matrix_type)
      case Matrix_Type is
         when Arff_Dense =>
            Values := Decode_Dense_Rows (Decoder, Text);
         when Arff_Coo => null;
            --              Values := Decode_COO_Rows (Decoder, Stream_Data'Access, Data,
            --                                         Rows, Cols);
         when others => null;
      end case;

      Arff_Container.Set_Field ("data", Values);
      Put_Line (Routine_Name & "done");

      return Arff_Container;

   end Decode_ARFF;

   --  -------------------------------------------------------------------------

   procedure Decode_Attribute (Decoder        : in out Arff_Decoder; UC_Row : String;
                               Arff_Container : in out JSON_Value) is
      use GNAT.Regpat;
      use Ada.Strings;
      --        use Ada.Strings.Maps;
      use Conversor_Item_Package;
      Routine_Name   : constant String := "ARFF.Decode_Attribute ";
      Regex          : constant String :=
                         "^("".*""|'.*'|[^\{\}%,\s]*)\s+(.+)$";
      --        Trim_Seq     : constant Character_Sequence := "{} ";
      --        Trim_Set     : constant Character_Set := To_Set (Trim_Seq);
      --  L749 Extract raw name and type
      Pos            : Positive := Fixed.Index (UC_Row, " ");
      Pos_1          : Natural;
      Slice_1        : constant String := UC_Row (UC_Row'First .. Pos - 1);
      Slice_2        : constant String :=
                         Fixed.Trim (UC_Row (Pos + 1 .. UC_Row'Last), Both);
      Slice_2_UB     : constant Unbounded_String := To_Unbounded_String (Slice_2);
      Slice_2_Last   : constant Positive := Slice_2'Last;
      H_Tab          : String (1 .. 1);
      Attr           : constant JSON_Value := Create_Object;
      Name           : Unbounded_String;
      Attr_Type      : Unbounded_String;
      Attr_Array     : JSON_Array;
      Nominal_Values : Indef_String_List;
      Conv_Item      : Conversor_Item;
      Curs           : Cursor := Decoder.Conversers.First;
      Found          : Boolean := False;
   begin
      Assert (Slice_1 = "@ATTRIBUTE", Routine_Name & "Declaration for " &
                Slice_1 & " but @ATTRIBUTE expected.");
      Assert (Match (Compile (Regex), Slice_2), Routine_Name &
                "attribute declaration '" & To_String (Attr_Type) &
                "' has an invalid format.");
      H_Tab (1) := ASCII.HT;

      --  L751 Extract the final name
      Pos := Slice_2'First;
      Pos_1 := Fixed.Index (Slice_2 (Pos .. Slice_2_Last), " ");
      if Pos_1 = 0 then
         Pos_1 := Fixed.Index (Slice_2 (Pos .. Slice_2_Last), H_Tab);
      end if;

      Assert (Pos_1 /= 0, Routine_Name & "Slice_2 separator not found.");
      Name := To_Unbounded_String (Slice (Slice_2_UB, 1, Pos_1 - 1));
      Pos := Pos_1 + 1;

      while Has_Element (Curs) and not Found loop
         Found := Element (Curs).Name = Name;
         Next  (Curs);
      end loop;

      Assert (not Found, Routine_Name & " duplicate attribute name: " &
                To_String (Name));

      --  L755 Extract the final type
      Attr_Type := To_Unbounded_String
        (Fixed.Trim (Slice (Slice_2_UB, Pos, Slice_2_Last), Both));
      if Slice (Attr_Type, 1, 1) = H_Tab then
         Attr_Type :=
           To_Unbounded_String (Slice (Attr_Type, 2, Length (Attr_Type)));
      end if;

      if Slice (Attr_Type, 1, 1) = "{" and
        Slice (Attr_Type, Length (Attr_Type), Length (Attr_Type)) = "}"
      then
         Attr_Type := To_Unbounded_String
           (Slice (Attr_Type, 2, Length (Attr_Type) - 1));
         Conv_Item.Data_Type := Conv_Nominal;
         Nominal_Values := Parse_Values (To_String (Attr_Type));
         Conv_Item.Nominal_List := Nominal_Values;
      else
         Attr_Type := Dataset_Utilities.To_Upper_Case (Attr_Type);
         Assert (Attr_Type = "NUMERIC" or Attr_Type = "REAL" or
                   Attr_Type = "INTEGER" or Attr_Type = "STRING",
                 Routine_Name & " invalid attribute type, " & Slice_2);
         if Attr_Type = "INTEGER" then
            Conv_Item.Data_Type := Conv_Integer;
         elsif Attr_Type = "NUMERIC" then
            Conv_Item.Data_Type := Conv_Numeric;
         elsif Attr_Type = "REAL" then
            Conv_Item.Data_Type := Conv_Real;
         elsif Attr_Type = "STRING" then
            Conv_Item.Data_Type := Conv_String;
         end if;
      end if;
      --  end Python _arff._decode_attribute

      Attr_Array := Get (Arff_Container, "attributes");
      Attr.Set_Field (To_String (Name), Attr_Type);
      Append (Attr_Array, Attr);
      Arff_Container.Set_Field ("attributes", Attr_Array);

      Conv_Item.Name := Name;
      Decoder.Conversers.Append (Conv_Item);

   end Decode_Attribute;

   --  -------------------------------------------------------------------------

   procedure Decode_Comment (UC_Row         : String;
                             Arff_Container : in out JSON_Value) is
      use GNAT.Regpat;
      --        Routine_Name : constant String := "ARFF.Decode_Comment ";
      Regex        : constant String := "^%(.*)";
      Matcher      : constant Pattern_Matcher := Compile (Regex);
      Matches      : Match_Array (0 .. Paren_Count (Matcher));
   begin
      Match (Matcher, UC_Row, Matches);
      declare
         Comment : constant String :=
                     UC_Row (Matches (1).First .. Matches (1).Last);
         aLine   : constant JSON_Value := Create_Object;
         Desc    : JSON_Array:= Arff_Container.Get ("description");
      begin
         aLine.Set_Field ("text", Comment);
         Append (Desc, aLine);
         Arff_Container.Set_Field ("description", Desc);
      end;

   end Decode_Comment;

   --  -------------------------------------------------------------------------

   procedure Decode_Relation (UC_Row         : String;
                              Arff_Container : in out JSON_Value) is
      use Ada.Strings;
      use GNAT.Regpat;
      Routine_Name : constant String := "ARFF.Decode_Relation ";
      Regex        : constant String := "^([^\{\}%,\s]*|"".*""|'.*')$";
      Pos          : Integer := Fixed.Index (UC_Row, " ");
      Slice_2      : constant String :=
                       Fixed.Trim (UC_Row (Pos + 1 .. UC_Row'Last), Both);
      UB_Slice     : Unbounded_String := To_Unbounded_String (Slice_2);
   begin
      Assert (Match (Compile (Regex), Slice_2),
              Routine_Name & " relation declaration '" & Slice_2 &
                "' has an invalid format");

      Pos := 1;
      while Pos > 0 and Pos < Length (UB_Slice) loop
         Pos := Fixed.Index (To_String (UB_Slice), """");
         if Pos > 0 and Pos < Length (UB_Slice) then
            if Slice (UB_Slice, Pos + 1, Pos + 1) = "'" then
               UB_Slice := To_Unbounded_String
                 (Slice (UB_Slice, 1, Pos - 1) &
                    Slice (UB_Slice, Pos + 2, Length (UB_Slice)));
            end if;
         end if;
      end loop;

      Arff_Container.Set_Field ("relation", UB_Slice);

   end Decode_Relation;

   --  -------------------------------------------------------------------------
   --  L531 COO refers to a matrix iCOOrdinate format
   --  which can be defined by:
   --  data[:] the entries of the matrix in any order
   --  i[:] the row indices of the matrix entries
   --  j[:] the column indices of the matrix entries
   --     function Decode_COO_Rows
   --       (Decoder : in Out Arff_Decoder; Stream_Func : Stream_Func_Type;
   --        Data    : out Classifier_Types.Float_List; Rows, Cols : out Integer_List)
   --        return String_List is
   --        use String_Package;
   --
   --        function "<" (L, R : Unbounded_String) return Boolean is
   --           L_String : constant String := To_String (L);
   --           R_String : constant String := To_String (R);
   --        begin
   --           return L_String < R_String;
   --        end "<";
   --        package String_Package_Sorting is new
   --          String_Package.Generic_Sorting ("<");
   --
   --        Routine_Name     : constant String := "ARFF.Decode_Rows_Dense ";
   --        --          Converser_Length : constant Natural := Natural (Decoder.Conversers.Length);
   --        Values         : String_List;
   --        Row_Col_Values : String_Vector;
   --        Curs           : String_Package.Cursor;
   --     begin
   --        --  L532
   --        while Has_Element (Stream_Cursor) loop
   --           declare
   --              Row : constant String := Stream_Func (Decoder);
   --           begin
   --              Values := Parse_Values (Row);
   --              if not Is_Empty (Values) then
   --                 String_Package_Sorting.Sort (Values);
   --                 --  L538
   --                 Curs := Values.First;
   --                 while Has_Element (Curs) loop
   --                    Row_Col_Values.Append (Element (Curs));
   --                    Next (Curs);
   --                 end loop;
   --              end if;
   --           end;
   --        end loop;
   --
   --        return Values;
   --
   --     end Decode_COO_Rows;

   --  -------------------------------------------------------------------------

   --  L460
   function Decode_Dense_Rows (Decoder : in out Arff_Decoder;
                               Stream  : String_List) return JSON_Array is
      use Ada.Calendar;
      use Ada.Strings;
      use String_Package;
      Routine_Name      : constant String := "ARFF.Decode_Dense_Rows ";
      Converser_Length  : constant Positive :=
                            Positive (Decoder.Conversers.Length);
      Row               : Unbounded_String;
      Values            : Indef_String_List;
      Values_Length     : Natural;
      Data_Values       : constant JSON_Value := Create_Object;
      Dense_Values      : JSON_Array;
      Result            : Unbounded_String := To_Unbounded_String ("");
      Values_Array      : JSON_Array;
      Start_Time        : Time;
      End_Time          : Time;
      Decode_Start_Time : Time;
      Decode_End_Time   : Time;
--         Count             : Natural := 0;
   begin
      Put_Line (Routine_Name & "Stream length" &
                  Integer'Image (Integer (Length (Stream))) & " rows");
      while Has_Element (Stream_Cursor) loop
--            Count := Count + 1;
--            Put_Line (Routine_Name & "Stream line" & Integer'Image (Count));
         --  L462  for row in stream:
         Row  := Element (Stream_Cursor);
         Trim (Row, Both);
         --           Put_Line (Routine_Name & To_String (Row));
         if Row /= "" then
            if Slice (Row, 1, 1) /= "%" and then
              Slice (Row, 1, 1) /= "@"
            then
               Result := Row;
               --  L463
               Start_Time := Clock;
               Values := Parse_Values (To_String (Result));
               End_Time := Clock;
                Put_Line (Routine_Name & "Parse time" &
                           Duration'Image (1000 * (End_Time - Start_Time)) &
                           " Milli_Sec");
--                 Values_Length := Natural (Length (Values));
               Values_Length := Natural (Values.Length);
               Clear (Dense_Values);
               Data_Values.Unset_Field ("values");
--                 Put_Line (Routine_Name & "Result: '" & To_String (Result) & "'");
               --              Put_Line (Routine_Name & "Values_Length: " & Integer'Image (Values_Length));
               --              delay (1.0);
               --              New_Line;

               if Length (Result) > 0 then
                  --                 Assert (not Values.Is_Empty, Routine_Name & "Row '" &
                  --                           Filtered_Row & "' has no valid values.");
                  Assert (Values_Length <= Converser_Length, Routine_Name &
                            "Row is invalid, Values length"  &
                            Integer'Image (Values_Length) &
                            " is different to Converser_Length" &
                            Integer'Image (Converser_Length));
                  Decode_Start_Time := Clock;
                  Dense_Values :=
                    Decode_Dense_Values (Values, Decoder.Conversers);
                  Decode_End_Time := Clock;
                  Put_Line (Routine_Name & "Decode_Dense_Values time" &
                            Duration'Image
                            (1000 * (Decode_End_Time - Decode_Start_Time)) &
                             " Milli_Sec");
                  Data_Values.Set_Field ("values", Dense_Values);
                  Append (Values_Array, Data_Values);
                  --                    Put_Line (Routine_Name & "Values_Array length" &
                  --                                Integer'Image (Length (Values_Array)));
               end if;
            end if;
         end if;

         Next (Stream_Cursor);
         delay (0.3);
      end loop;

      Put_Line (Routine_Name & "Values_Array length" &
                  Integer'Image (Length (Values_Array)));

      --  L475
      return Values_Array;

   end Decode_Dense_Rows;

   --  -------------------------------------------------------------------------
   --  L478
   function Decode_Dense_Values (Values         : Indef_String_List;
                                 Attribute_List : Conversor_Item_List)
                                 return JSON_Array is
      use Ada.Containers;
      use Ada.Strings;
      use Conversor_Item_Package;
      use Indefinite_String_Package;
      Routine_Name   : constant String := "ARFF.Decode_Dense_Values ";
      Attr_Cursor    : Conversor_Item_Package.Cursor;
      Values_Cursor  : Indefinite_String_Package.Cursor;
      Nominal_Cursor : Indefinite_String_Package.Cursor;
      aConverser     : Conversor_Item;
      Data_Type      : Conversor_Data_Type;
      Decoded_Values : JSON_Array;
      Found          : Boolean := False;
   begin
      Assert (Values.Length = Attribute_List.Length, Routine_Name &
                "lengths of Values " & Count_Type'Image (Values.Length) &
                " and Conversers" & Count_Type'Image (Attribute_List.Length) &
                " are diferrent.");

      Attr_Cursor := Attribute_List.First;
      Values_Cursor := Values.First;

      while Has_Element (Attr_Cursor) loop
         aConverser := Element (Attr_Cursor);
         Data_Type := aConverser.Data_Type;
         declare
            Name           : constant String := To_String (aConverser.Name);
            Value          : constant JSON_Value := Create_Object;
            Value_String   : constant String := Element (Values_Cursor);
            UC_Value       : constant String :=
                               To_Upper_Case (Value_String);
         begin
--              Put_Line (Routine_Name & "Conversor Data_Type " &
--                          Conversor_Data_Type'Image (Data_Type));
--              Put_Line (Routine_Name & "Value_String: " & Value_String);
            case Data_Type is
               when Conv_Integer =>
                  Put_Line (Routine_Name & "Data_Type Conv_Integer");
                  if Fixed.Index (Value_String, ".") = 0 then
                     Value.Set_Field
                       (Name, Integer'Value (Value_String));
                  else
                     Value.Set_Field
                       (Name, Integer (Float'Value (Value_String)));
                  end if;

               when Conv_Nominal =>
                  Nominal_Cursor := aConverser.Nominal_List.First;
                  while Has_Element (Nominal_Cursor) and not Found loop
                     declare
                        Nominal_String : constant String
                          := Element (Nominal_Cursor);
                     begin
                        Found := UC_Value = Nominal_String;
                     end;
                     Next (Nominal_Cursor);
                  end loop;

                  Assert (Found, Routine_Name & UC_Value &
                            " is an invalid nominal type");
                  Value.Set_Field ("nominal type", UC_Value);

               when Conv_Numeric | Conv_Real =>
--                    Put_Line (Routine_Name & "Conv_Real Value_String: " &
--                                Value_String);
                  if Fixed.Index (Value_String, ".") = 0 then
                     Value.Set_Field
                       (Name, Float (Integer'Value (Value_String)));
                  else
                     Value.Set_Field (Name, Float'Value (Value_String));
                  end if;

               when Conv_String =>
                  Value.Set_Field (Name, Value_String);

            end case;
            Append (Decoded_Values, Value);
         end;  --  declare block

         Next (Attr_Cursor);
         Next (Values_Cursor);
      end loop;

      return Decoded_Values;

   end Decode_Dense_Values;

   --  -------------------------------------------------------------------------
   --  match.group returns one or more subgroups of the match.
   --  If there is a single argument, the result is a single string;
   --  if there are multiple arguments, the result is a tuple with one item
   --  per argument.
   --  Without arguments, group1 defaults to zero (the whole match is returned).
   function Escape_Sub_Callback (Match : Regexep.Match_Strings_List)
                                 return String is
      use Ada.Strings;
      use Regexep;
      use Escape_Sub_Map_Package;
      Routine_Name : constant String := "ARFF.Escape_Sub_Callback ";
      Match_Groups : constant Match_Strings_List := Get_Groups (Match);
      S            : constant Unbounded_String := Match_Groups.Element (0);
      S_Length     : constant Natural := Length (S);
      Int_Value    : Integer;
      Based_Int    : String (1 .. 2 * S_Length);
      Result       : Unbounded_String;
   begin
      if S_Length = 4 then
         Assert (Escape_Sub_Map.Contains (S), Routine_Name &
                   "Unsupported escape sequence: " & To_String (S));
         Result := Escape_Sub_Map.Element (S);

      elsif To_String (S) (2) = 'u' then
         Int_Value := Integer'Value (Slice (S, 3, S_Length));
         Ada.Integer_Text_IO.Put (Based_Int, Int_Value, 16);
         Result := Trim (To_Unbounded_String (Based_Int), Both);
      else
         Int_Value := Integer'Value (Slice (S, 2, S_Length));
         Ada.Integer_Text_IO.Put (Based_Int, Int_Value, 8);
         Result := Trim (To_Unbounded_String (Based_Int), Both);
      end if;

      return To_String (Result);

   end Escape_Sub_Callback;
   pragma Unreferenced (Escape_Sub_Callback);

   --  -------------------------------------------------------------------------

   function Escape_Sub_Callback (S : String) return String is
      use Ada.Strings;
      use Escape_Sub_Map_Package;
      Routine_Name : constant String := "ARFF.Escape_Sub_Callback ";
      UB_S         : constant Unbounded_String := To_Unbounded_String (S);
      S_Length     : constant Natural := S'Length;
      Int_Value    : Integer;
      Based_Int    : String (1 .. 2 * S_Length);
      Result       : Unbounded_String;
   begin
      if S_Length = 4 then
         Assert (Escape_Sub_Map.Contains (UB_S), Routine_Name &
                   "Unsupported escape sequence: " & S);
         Result := Escape_Sub_Map.Element (UB_S);

      elsif S (S'First + 1) = 'u' then
         Int_Value := Integer'Value (Slice (UB_S, 3, S_Length));
         Ada.Integer_Text_IO.Put (Based_Int, Int_Value, 16);
         Result := Trim (To_Unbounded_String (Based_Int), Both);
      else
         Int_Value := Integer'Value (Slice (UB_S, 2, S_Length));
         Ada.Integer_Text_IO.Put (Based_Int, Int_Value, 8);
         Result := Trim (To_Unbounded_String (Based_Int), Both);
      end if;

      return To_String (Result);

   end Escape_Sub_Callback;

   --  -------------------------------------------------------------------------

   --     function Load (File_Data   : String;
   --                    Return_Type : ARFF_Return_Type := Arff_Dense)
   --                    return JSON_Value is
   --        Decoder   : Arff_Decoder;
   --     begin
   --        return Decode_ARFF (Decoder, File_Data,  Return_Type);
   --
   --     end Load;

   --  -------------------------------------------------------------------------

   function Load (File_Data   : ML_Types.String_List;
                  Return_Type : ARFF_Return_Type := Arff_Dense)
                  return JSON_Value is
      Decoder   : Arff_Decoder;
   begin
      return Decode_ARFF (Decoder, File_Data,  Return_Type);

   end Load;

   --  -------------------------------------------------------------------------
   --  L283 Parse_Values splits a line into a list of values
   --  Match produces Matches of type Match_Array.
   --  Each component of Matches is set to the subrange of the
   --  matches substring or to No_Match if no match.
   --  Matches (N) is for the  N'th parenthesized subexpressions;
   --  Matches (0) is for the whole expression.
   function Parse_Values (Row : String) return Indef_String_List is
--        use Ada.Calendar;
      use GNAT.Regpat;
      use Regexep;
      use Indefinite_String_Package;
      use String_Package;
      Routine_Name        : constant String := "ARFF.Parse_Values ";
      Non_Trivial         : constant String := "[""\'{}\\s]";
      Non_Trivial_Matcher : constant Pattern_Matcher := Compile (Non_Trivial);
      First               : Positive;
      Last                : Positive;
      Match_Found         : Boolean;
      Dense_Match         : Boolean;
      Sparse_Match        : Boolean;
      Matches             : Matches_List;
      Values              : Indef_String_List;
      Value_Cursor        : Indefinite_String_Package.Cursor;
      Errors              : String_List;
      Result              : String_List;
--        Start_Time          : Time;
--        End_Time            : Time;
   begin
      if Row'Length /= 0 and then Row /= "?" then
         --           Put_Line (Routine_Name & "Row: '" & Row & "'");
         --           Start_Time := Clock;
         Matches := Find_Match (Non_Trivial_Matcher, Row, First, Last,
                                Match_Found);
         --           End_Time := Clock;
         --           Put_Line (Routine_Name & "Non_Trivial Match time" &
         --                     Duration'Image (1000 * ( End_Time - Start_Time)) &
         --                     " Milli_Sec");
         pragma Unreferenced (Matches);
         if Match_Found then
            Put_Line (Routine_Name & "trivial");
            Put_Line (Routine_Name & "data contains "", ', { ,} or white space");
            --  not nontrivial
            --  Row contains none of the Non_Trivial characters
            Values := Split_CSV (Row);
--              Put_Line (Routine_Name & "trivial Values length:" &
--                          Integer'Image (Integer (Length (Values))));

         else
            --              Put_Line (Routine_Name & "nontrivial");
            --  Row contains Non_Trivial characters
            --  Build_Re_Dense and Build_Re_Sparse (_RE_DENSE_VALUES) tokenize
            --  despite quoting, whitespace, etc.
            declare
               Dense_Matcher  : constant Pattern_Matcher := Build_Re_Dense;
               Sparse_Matcher : constant Pattern_Matcher := Build_Re_Sparse;
            begin
               Matches := Find_Match
                 (Dense_Matcher, Row, First, Last, Dense_Match);
               if Dense_Match then
--                    Start_Time := Clock;
                  Values := Split_CSV (Row (First .. Last));
--                    End_Time := Clock;
--                    Put_Line (Routine_Name & "split string execution time" &
--                                Duration'Image (1000 * ( End_Time - Start_Time)) &
--                                " Milli_Sec");
--                    Put_Line (Routine_Name & "Row (First .. Last):" &
--                                Row (First .. Last));
--                    Put_Line (Routine_Name & "dense Values length:" &
--                                Integer'Image (Integer (Length (Values))));
               else
                  Matches := Find_Match (Sparse_Matcher, Row, First, Last,
                                         Sparse_Match);
                  if Sparse_Match then
                     Put_Line (Routine_Name & "Sparse_Match");
                     Errors := Split_String (Row (First .. Last), ",");
                     if not Errors.Is_Empty then
                        Value_Cursor := Values.First;
                        while Has_Element (Value_Cursor) loop
                           Result.Append (Unquote (Element (Value_Cursor)));
                           Next (Value_Cursor);
                        end loop;

                     else
                        Values := Split_Sparse_Line (Row);
                     end if;
                  end if;
               end if;
            end;  --  declare block

         end if;
      end if;

      return Values;

   end Parse_Values;

   --  -------------------------------------------------------------------------

   function Split_Sparse_Line (Row : String) return Indef_String_List is
      use GNAT.Regpat;
      use Regexep;
      use Matches_Package;
      Matcher     : constant Pattern_Matcher := Build_Re_Sparse;
      First       : Positive;
      Last        : Positive;
      Matches     : Matches_List;
      Loc         : Match_Location;
      Match_Found : Boolean;
      Result      : Indef_String_List;
   begin
      Matches := Find_Match (Matcher, Row, First, Last, Match_Found);
      if Match_Found then
         for index in Matches.First_Index .. Matches.Last_Index loop
            Loc := Matches.Element (index);
            declare
               Match    : constant String := Row (Loc.First .. Loc.Last);
            begin
               Result.Append (Unquote (Match));
            end;
         end loop;
      end if;

      return Result;

   end Split_Sparse_Line;

   --  -------------------------------------------------------------------------

   function Unquote (Values : String) return String is
      use Regexep;
      --  \[0-9]{1,3} match when \ is followed by 1 to 3 digits
      --  \u[0-9a-f]{4} match string starting with \u followed by 4 hex digits
      --  \. match \.
      --  In each case first to last refers to the characters follwing the /
      Pattern       : constant String := "\\([0-9]{1,3}|u[0-9a-f]{4}|.)";
   begin
      if Values = "" or  Values (Values'First) = '?' then
         return "";
      elsif Values (Values'First) = '"' or Values (Values'First) = ''' then
         return Substitute (Values (Values'First + 1 .. Values'Last - 1),
            Pattern, Escape_Sub_Callback'Access);
      else
         return Values;
      end if;

   end Unquote;

   --  -------------------------------------------------------------------------

   function Unquote (Values : String) return Unbounded_String is
      use Regexep;
      --  \[0-9]{1,3} match when \ is followed by 1 to 3 digits
      --  \u[0-9a-f]{4} match string starting with \u followed by 4 hex digits
      --  \. match \.
      --  In each case first to last refers to the characters follwing the /
      Pattern       : constant String := "\\([0-9]{1,3}|u[0-9a-f]{4}|.)";
      Result        : Unbounded_String := To_Unbounded_String ("");
   begin
      if Values = "" or  Values (Values'First) = '?' then
         null;
      elsif Values (Values'First) = '"' or Values (Values'First) = ''' then
         Result := To_Unbounded_String
           (Substitute (Values (Values'First + 1 .. Values'Last - 1),
            Pattern, Escape_Sub_Callback'Access));
      else
         Result := To_Unbounded_String (Values);
      end if;

      return Result;

   end Unquote;

   --  -------------------------------------------------------------------------

   use Escape_Sub_Map_Package;
   --      Escape_Key : Unbounded_String;
   --      Escape_Val : Unbounded_String;
begin
   Escape_Sub_Map.Include (To_Unbounded_String ("\\\\"),
                           To_Unbounded_String ("\\"));
   Escape_Sub_Map.Include (To_Unbounded_String ("\\"""),
                           To_Unbounded_String (""""));
   Escape_Sub_Map.Include (To_Unbounded_String ("\\'"),
                           To_Unbounded_String ( "'"));
   Escape_Sub_Map.Include (To_Unbounded_String ("\\t"),
                           To_Unbounded_String ("\t"));
   Escape_Sub_Map.Include (To_Unbounded_String ("\\n"),
                           To_Unbounded_String ("\n"));
   Escape_Sub_Map.Include (To_Unbounded_String ("\\r"),
                           To_Unbounded_String ("\r"));
   Escape_Sub_Map.Include (To_Unbounded_String ("\\b"),
                           To_Unbounded_String ("\b"));
   Escape_Sub_Map.Include (To_Unbounded_String ("\\f"),
                           To_Unbounded_String ("\f"));
   Escape_Sub_Map.Include (To_Unbounded_String ("\\%"),
                           To_Unbounded_String ("%"));
   Escape_Sub_Map.Include (To_Unbounded_String ("\\0"),
                           To_Unbounded_String ("\x00"));
   Escape_Sub_Map.Include (To_Unbounded_String ("\\1"),
                           To_Unbounded_String ("\x01"));
   Escape_Sub_Map.Include (To_Unbounded_String ("\\2"),
                           To_Unbounded_String ("\x02"));
   Escape_Sub_Map.Include (To_Unbounded_String ("\\3"),
                           To_Unbounded_String ("\x03"));
   Escape_Sub_Map.Include (To_Unbounded_String ("\\4"),
                           To_Unbounded_String ("\x04"));
   Escape_Sub_Map.Include (To_Unbounded_String ("\\5"),
                           To_Unbounded_String ("\x05"));
   Escape_Sub_Map.Include (To_Unbounded_String ("\\6"),
                           To_Unbounded_String ("\x06"));
   Escape_Sub_Map.Include (To_Unbounded_String ("\\7"),
                           To_Unbounded_String ("\x07"));
   Escape_Sub_Map.Include (To_Unbounded_String ("\\8"),
                           To_Unbounded_String ("\x08"));
   Escape_Sub_Map.Include (To_Unbounded_String ("\\9"),
                           To_Unbounded_String ("\t"));

end ARFF;
