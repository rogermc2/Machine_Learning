--  Based on scikit-learn/sklearn/externals _arff.py

with Ada.Assertions; use Ada.Assertions;
with Ada.Containers.Doubly_Linked_Lists;
with Ada.Containers.Ordered_Maps;
with Ada.Integer_Text_IO;
with Ada.Strings.Fixed;
with Ada.Strings.Maps;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
--  with Ada.Strings.Unbounded.Text_IO; use Ada.Strings.Unbounded.Text_IO;
--  with Ada.Text_IO; use Ada.Text_IO;

with GNAT.Regpat;

with Dataset_Utilities; use Dataset_Utilities;
with ML_Types; use ML_Types;
with Regexep;

pragma Warnings (Off);

package body ARFF is

   type TK_State is (TK_Descrition, TK_Comment, TK_Relation, TK_Attribute,
                     TK_Data);
   type Conversor_Type is (Conversor_Unencoded, Conversor_Encoded,
                           Conversor_Map);

   type Conversor_Data (Encoded : Conversor_Type := Conversor_Map)
   is record
      case Encoded is
         when Conversor_Unencoded =>
            Values         : ML_Types.String_List;
         when Conversor_Encoded =>
            Encoded_Values : ML_Types.String_List;
            Zero_Value     : Unbounded_String := To_Unbounded_String ("");
         when Conversor_Map =>
            Mapping        : Unbounded_String := To_Unbounded_String ("");
      end case;
   end record;

   type Converser_Map is record
      Text    : Unbounded_String := To_Unbounded_String ("");
      Int     : Integer := 0;
      Numeric : Float := 0.0;
      Real    : Float := 0.0;
   end record;

   package Conversor_Package is new
     Ada.Containers.Doubly_Linked_Lists (Conversor_Data);
   subtype Conversor_List is Conversor_Package.List;

   type Conversor_Tuple is record
      Key   : Conversor_Data;
      Value : Unbounded_String;
   end record;

   package Conversor_Tuple_Package is new
     Ada.Containers.Doubly_Linked_Lists (Conversor_Tuple);
   subtype Conversor_Tuple_List is Conversor_Tuple_Package.List;

   type Arff_Decoder is record
      Conversers   : Conversor_List;
      Current_Line : Integer := 0;
   end record;

   package Escape_Sub_Map_Package is new
     Ada.Containers.Ordered_Maps (Unbounded_String, Unbounded_String);

   type Stream_Func_Type is access function (Decoder : in out Arff_Decoder)
                                             return String;

   Escape_Sub_Map : Escape_Sub_Map_Package.Map;
   Stream_Cursor  : ML_Types.String_Package.Cursor;
   Quoted_Re      : constant String :=
                      "''""(?:(?<!\\)(?:\\\\)*\\""|\\|[^']|[^""\\])*""";
   Quoted_Re2     : constant String :=
                      "'''(?:(?<!\\)(?:\\\\)*\\'|\\[^']|[^'\\])*''";
   Value_Re       : constant String := "''(?:''" & Quoted_Re & "|" &
                      Quoted_Re2 & "|[^,\s""'{}]+)''";

   procedure Decode_Attribute (Decoder         : in Out Arff_Decoder;
                               UC_Row          : String;
                               Encode_Nominal  : Boolean := False;
                               Attribute_Names : in out JSON_Value;
                               Arff_Container  : in out JSON_Value);
   procedure Decode_Comment
     (UC_Row : String; Arff_Container : in out JSON_Value);
   procedure Decode_Relation
     (UC_Row : String; Arff_Container : in out JSON_Value);
   procedure Decode_Rows_COO (Stream         : ML_Types.String_List;
                              Arff_Container : in out JSON_Value);
   function Decode_Rows_Dense (Decoder     : in Out Arff_Decoder;
                               Stream_Func : Stream_Func_Type)
                               return String_List;
   function Decode_Values (Values : String_List; Conversers : Conversor_List)
                           return String_List;
   function Max_Value (Values : String_List) return Integer;
   function Parse_Values (Row : String) return String_List;
   function Stream_Data (Decoder : in out Arff_Decoder) return String;
   function Unquote (Values : String) return String;

   --  -------------------------------------------------------------------------
   --  Build_Re_Dense and Build_Re_Sparse (_RE_DENSE_VALUES) tokenize
   --  despite quoting, whitespace, etc.
   function Build_Re_Dense return GNAT.Regpat.Pattern_Matcher is
      use GNAT.Regpat;
      --  "         open quote followed by zero or more:
      --  (?:
      --  (?<!\\)    no additional backslash
      --  (?:\\\\)*  maybe escaped backslashes
      --  \\"        escaped quote
      --  \\[^"]     escaping a non-quote
      --  [^"\\]     non-quote char
      --  )*
      --  "          close quote
   begin
      --  Dense captures (value, error) groups.
      --  Because empty values are allowed, we cannot just look for empty
      --  values to handle syntax errors.
      --  We presume the line has had ',' prepended...
      return Compile ("''(?x),\s*((?=,)|$|{" & Value_Re & "})|(\S.*)''");

   end Build_Re_Dense;

   --  -------------------------------------------------------------------------

   function Build_Re_Sparse return GNAT.Regpat.Pattern_Matcher is
      use GNAT.Regpat;
   begin
      return Compile ("(?x)(?:^\s*\{|,)\s*(\d+)\s+(" & Value_Re &
                        "s)|(?!}\s*$)(?!^\s*{\s*}\s*$)\S.*");

   end Build_Re_Sparse;

   --  -------------------------------------------------------------------------
   --  L771
   function Decode (Decoder     : in Out Arff_Decoder;
                    Text        : String; Encode_Nominal : Boolean := False;
                    Matrix_Type : ARFF_Return_Type := Arff_Dense)
                    return JSON_Value is
      use Ada.Strings;
      use ML_Types.String_Package;
      Routine_Name    : constant String := "ARFF.Decode ";
      Bad_Layout      : constant String := " layout of ARFF file is bad.";
      Text_Length     : constant Integer := Text'Length;
      State           : TK_State := TK_Descrition;
      Pos1            : Integer := 1;
      Pos2            : Integer := 1;
      Message_Lines   : String_List;
      Curs            : Cursor;
      Arff_Container  : JSON_Value := Create_Object;
      ARFF_Data       : JSON_Value;
      Data            : JSON_Value;
      Attribute_Names : JSON_Value := Create_Object;
      Stream_Curs     : Cursor := Message_Lines.First;
      Stream_Row      : Unbounded_String;
      Values          : String_List;
   begin
      Decoder.Current_Line := 0;
      while Pos2 /= 0 and Pos1 < Text_Length loop
         Pos2 := Fixed.Index (Text, "\r\n");
         Message_Lines.Append
           (To_Unbounded_String
              (Fixed.Trim (Text (Pos1 .. Pos2 - 1), Both)));
         Pos1 := Pos2 + 3;
      end loop;
      if Pos1 < Text_Length - 4 then
         Message_Lines.Append
           (To_Unbounded_String (Text (Pos1 .. Text_Length - 4)));
      end if;

      Arff_Container.Set_Field ("description", "");
      Arff_Container.Set_Field ("relation", "");
      Arff_Container.Set_Field ("attributes", Create (Empty_Array));
      Arff_Container.Set_Field ("description", Create (Empty_Array));
      --        Data := Get_Data_Object_For_Decoding (Matrix_Type);

      --  L796
      Curs := Message_Lines.First;
      while Has_Element (Curs) loop
         Decoder.Current_Line := Decoder.Current_Line + 1;
         declare
            UC_Row : constant String :=
                       Dataset_Utilities.To_Upper_Case
                         (To_String (Element (Curs)));
         begin
            if UC_Row /= "" then
               if UC_Row = "@RELATION" then
                  Assert (State = TK_Descrition,
                          Routine_Name & Bad_Layout);
                  State := TK_Relation;
                  Decode_Relation (UC_Row, Arff_Container);
               elsif UC_Row = "@ATTRIBUTE" then
                  Assert (State = TK_Relation or State = TK_Attribute,
                          Routine_Name & Bad_Layout);
                  State := TK_Attribute;
                  Decode_Attribute (Decoder, UC_Row, Encode_Nominal,
                                    Attribute_Names, Arff_Container);

               elsif UC_Row = "@DATA" then
                  Assert (State = TK_Attribute, Routine_Name & Bad_Layout);
               elsif UC_Row (1 .. 1) = "%" then
                  Decode_Comment (UC_Row, Arff_Container);
               else
                  Assert (False, Routine_Name & Bad_Layout);
               end if;
            end if;
         end;

         Next (Curs);
      end loop;

      --  L872
      case Matrix_Type is
         when Arff_Dense =>
            Values := Decode_Rows_Dense (Decoder, Stream_Data'Access);
         when others => null;
      end case;

      Stream_Row := Get (ARFF_Data, "description");
      if Tail (Stream_Row, 2) = "\n" then
         Stream_Row := To_Unbounded_String
           (Slice (Stream_Row, 1, Length (Stream_Row) - 2));
         ARFF_Data.Set_Field ("description", Stream_Row);
      end if;

      return ARFF_Data;

   end Decode;

   --  -------------------------------------------------------------------------

   procedure Decode_Attribute (Decoder         : in Out Arff_Decoder;
                               UC_Row          : String;
                               Encode_Nominal  : Boolean := False;
                               Attribute_Names : in out JSON_Value;
                               Arff_Container  : in out JSON_Value) is
      use Ada.Strings;
      use Ada.Strings.Maps;
      use GNAT.Regpat;
      Routine_Name : constant String := "ARFF.Decode_Relation ";
      Regex        : constant String :=
                       "^("".*""|'.*'|[^\{\}%,\s]*)\s+(.+)$";
      Trim_Seq     : constant Character_Sequence := "{} ";
      Trim_Set     : constant Character_Set := To_Set (Trim_Seq);
      --        Slices       : Array (1 .. 2) of Unbounded_String;
      --  L749 Extract raw name and type
      Pos          : Integer := Fixed.Index (UC_Row, " ");
      Slice_1      : constant String := UC_Row (UC_Row'First .. Pos - 1);
      Slice_2      : String :=
                       Fixed.Trim (UC_Row (Pos + 1 .. UC_Row'Last), Both);
      Name         : Unbounded_String;
      Attr_Type    : Unbounded_String;
      Attribute    : JSON_Value := Create_Object;
      Values       : String_List;
      Converser    : Conversor_Data;
   begin
      Name := To_Unbounded_String (Slice_1);
      Assert (Match (Compile (Regex), Slice_2),
              Routine_Name & " attribute declaration '" &
                To_String (Attr_Type) &
                "' has an invalid format");

      --  L751 Extract the final name
      Pos := 1;
      while Pos > 0 and Pos < Length (Name) loop
         Pos := Fixed.Index (To_String (Name), """");
         if Pos > 0 and Pos < Length (Name) then
            if Slice (Name, Pos + 1, Pos + 1) = "'" then
               Name := To_Unbounded_String
                 (Slice (Name, 1, Pos - 1) &
                    Slice (Name, Pos + 2, Length (Name)));
            end if;
         end if;
      end loop;

      --  L755 Extract the final type
      Attr_Type := To_Unbounded_String (Slice_2);
      if Slice_2 (Slice_2'First) = '{' and Slice_2 (Slice_2'Last) = '{' then
         Attr_Type := To_Unbounded_String
           (Fixed.Trim (Slice_2, Left => Trim_Set, Right => Trim_Set));
         Values := Parse_Values (To_String (Attr_Type));
      else
         Slice_2 := Dataset_Utilities.To_Upper_Case (Slice_2);
         Assert (Slice_2 = "NUMERIC" or Slice_2 = "REAL" or
                   Slice_2 = "INTEGER" or Slice_2 = "STRING",
                 Routine_Name & " invalid attribute type, " & Slice_2);
      end if;

      declare
         Attr_Name : constant String := To_String (Name);
      begin
         Assert (not Attribute_Names.Has_Field
                 (Attr_Name), Routine_Name &
                   " duplicate attribute name: " & Attr_Name);
         Attribute_Names.Set_Field
           (Attr_Name, Trimmed_Integer (Decoder.Current_Line));

         Attribute.Set_Field (Attr_Name, To_String (Attr_Type));
         Arff_Container.Set_Field ("attributes", Attribute);

         --  L832
         if Kind (Get (Attribute, Attr_Name)) = JSON_Array_Type then
            if Encode_Nominal then
               declare
                  Converser : Conversor_Data (Conversor_Encoded);
                  Values    : String_List;
               begin
                  Values.Append (Get (Attribute, "attributes"));
                  Converser.Encoded_Values := Values;
                  Decoder.Conversers.Append (Converser);
               end;

            else
               declare
                  Converser : Conversor_Data (Conversor_Unencoded);
                  Values    : String_List;
               begin
                  Values.Append (Get (Attribute, "attributes"));
                  Converser.Values := Values;
                  Decoder.Conversers.Append (Converser);
               end;
            end if;

         else
            declare
               Converser     : Conversor_Data (Conversor_Encoded);
               Converser_Map : JSON_Value := Create_Object;
               Lambda        : JSON_Value := Create_Object;
            begin
               --  L838
               Converser_Map.Set_Field ("STRING", "");
               Converser_Map.Set_Field ("INTEGER", Integer (0));
               Converser_Map.Set_Field ("NUMERIC", 0.0);
               Converser_Map.Set_Field ("REAL", 0.0);

               --                 Converser := Element (Converser_Map, "INTEGER");
               Decoder.Conversers.Append (Converser);
            end;
         end if;
      end;

   end Decode_Attribute;

   --  -------------------------------------------------------------------------

   procedure Decode_Comment (UC_Row         : String;
                             Arff_Container : in out JSON_Value) is
      use GNAT.Regpat;
      Regex   : constant String := "^\%( )?";
      Matches : Match_Array (0 .. 1);
   begin
      Match (Compile (Regex), UC_Row, Matches);
      declare
         Comment : constant String :=
                     UC_Row (Matches (1).First .. Matches (1).Last)
                     & "\n";
         Desc    : constant String :=
                     Arff_Container.Get ("description") & Comment;
      begin
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
      --        Slices       : Array (1 .. 2) of Unbounded_String;
      Pos          : Integer := Fixed.Index (UC_Row, " ");
      --        Slice_1      : constant String := UC_Row (1 .. Pos - 1);
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

   procedure Decode_Rows_COO (Stream         : String_List;
                              Arff_Container : in out JSON_Value) is

   begin
      null;
   end Decode_Rows_COO;

   --  -------------------------------------------------------------------------
   --  L460
   function Decode_Rows_Dense (Decoder     : in Out Arff_Decoder;
                               Stream_Func : Stream_Func_Type)
                               return String_List is
      use String_Package;
      Routine_Name     : constant String := "ARFF.Decode_Rows_Dense ";
      Converser_Length : constant Natural := Natural (Decoder.Conversers.Length);
      Row              : String := Stream_Func (Decoder);
      Values           : String_List := Parse_Values (Row);
   begin
      Assert (not Values.Is_Empty and Max_Value (Values) < Converser_Length ,
              Routine_Name & Row & " format is invalid.");

      return Decode_Values (Values, Decoder.Conversers);

   end Decode_Rows_Dense;

   --  -------------------------------------------------------------------------

   function Decode_Values (Values     : String_List;
                           Conversers : Conversor_List) return String_List is
      use Ada.Containers;
      use Conversor_Package;
      use String_Package;
      use Conversor_Tuple_Package;

      Routine_Name   : constant String := "ARFF.Decode_Values ";
      Zip_Values     : Conversor_Tuple_List;
      Conv_Cursor    : Conversor_Package.Cursor;
      Values_Cursor  : String_Package.Cursor;
      Zip_Cursor     : Conversor_Tuple_Package.Cursor;
      Decoded_Values : String_List;
   begin
      Assert (Values.Length = Conversers.Length, Routine_Name &
                "lengths of Values " & Count_Type'Image (Values.Length) &
                " and Conversers" & Count_Type'Image (Conversers.Length) &
                " are diferrent.");

      Conv_Cursor := Conversers.First;
      Values_Cursor := Values.First;

      while Has_Element (Conv_Cursor) loop
         Zip_Values.Append
           ((Element (Conv_Cursor), Element (Values_Cursor)));
         Next (Conv_Cursor);
         Next (Values_Cursor);
      end loop;

      Zip_Cursor := Zip_Values.First;
      while Has_Element (Zip_Cursor) loop
         Decoded_Values.Append (Element (Zip_Cursor).Value);
         Next (Zip_Cursor);
      end loop;

      return Decoded_Values;

   end Decode_Values;

   --  -------------------------------------------------------------------------

   function Max_Value (Values : String_List) return Integer is
      use String_Package;
      Curs : Cursor := Values.First;
      Val  : Integer;
      Max  : Integer := -Integer'Last;
   begin
      while Has_Element (Curs) loop
         Val := Integer'Value (To_String (Element (Curs)));
         if Val > Max then
            Max := Val;
         end if;
      end loop;

      return Max;

   end Max_Value;

   --  -------------------------------------------------------------------------

   procedure Init_Nominal_Conversor (Conversor : in out Conversor_Data;
                                     Values    : String_List) is
   begin
      Conversor.Values := Values;
      if Conversor.Encoded = Conversor_Unencoded then
         Conversor.Zero_Value := Values.First_Element;
      end if;

   end Init_Nominal_Conversor;

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

   procedure Init_Nominal_Conversor (Conversor : in out Conversor_Data;
                                     Value     : String) is
   begin
      Conversor.Values.Clear;
      Conversor.Zero_Value := To_Unbounded_String (Value);
      Conversor.Values.Append (To_Unbounded_String (Value));

   end Init_Nominal_Conversor;

   --  -------------------------------------------------------------------------

   function Load (File_Data   : String; Encode_Nominal : Boolean := False;
                  Return_Type : ARFF_Return_Type := Arff_Dense)
                  return JSON_Value is
      Decoder   : Arff_Decoder;
      ARFF_Data : constant JSON_Value :=
                    Decode (Decoder, File_Data, Encode_Nominal, Return_Type);
   begin
      return ARFF_Data;

   end Load;

   --  -------------------------------------------------------------------------
   --  L283 Parse_Values splits a line into a list of values
   --  Match produces Matches of type Match_Array.
   --  Each component of Matches is set to the subrange of the
   --  matches substring or to No_Match if no match.
   --  Matches (N) is for the  N'th parenthesized subexpressions;
   --  Matches (0) is for the whole expression.
   function Parse_Values (Row : String) return String_List is
      use GNAT.Regpat;
      use Regexep;
      use String_Package;
      Non_Trivial  : constant String := "[""\'{}\\s]";
      First        : Positive;
      Last         : Positive;
      Match_Found  : Boolean;
      Dense_Match  : Boolean;
      Sparse_Match : Boolean;
      Matches      : Matches_List;
      Values       : String_List;
      Errors       : String_List;
   begin
      if Row'Length /= 0 and then Row /= "?" then
         Matches := Find_Match (Compile (Non_Trivial), Row, First, Last, Match_Found);
         if Match_Found then
            --  not nontrivial
            --  Row contains none of the Non_Trivial characters
            Values := Get_CSV_Data (Row);
         else
            --  Row contains Non_Trivial characters
            --  Build_Re_Dense and Build_Re_Sparse (_RE_DENSE_VALUES) tokenize
            --  despite quoting, whitespace, etc.
            declare
               Dense  : GNAT.Regpat.Pattern_Matcher := Build_Re_Dense;
               Sparse : GNAT.Regpat.Pattern_Matcher := Build_Re_Sparse;
            begin
               Matches := Find_Match (Dense, Row, First, Last, Dense_Match);
               if Dense_Match then
                  Values := Get_CSV_Data (Row (First .. Last));
               end if;
               Matches := Find_Match (Sparse, Row, First, Last, Sparse_Match);
               if Sparse_Match then
                  Errors := Get_CSV_Data (Row (First .. Last));
               end if;
            end;  --  declare block

            if not Errors.Is_Empty then
               null;
            else
               null;
            end if;
         end if;
      end if;

      return Values;

   end Parse_Values;

   --  -------------------------------------------------------------------------

   function Stream_Data (Decoder : in out Arff_Decoder) return String is
      use Ada.Strings;
      use String_Package;
      Row    : Unbounded_String;
      Result : Unbounded_String;
   begin
      while Has_Element (Stream_Cursor) loop
         Decoder.Current_Line := Decoder.Current_Line + 1;
         Row := Element (Stream_Cursor);
         Trim (Row, Both);
         declare
            UC_Row : String := Dataset_Utilities.To_Upper_Case
              (To_String (Row));
         begin
            if Length (Row) > 0 and UC_Row (1 .. 1) /= "%" then
               Result := Row;
            end if;
         end;
         Next (Stream_Cursor);
      end loop;

      return To_String (Result);

   end Stream_Data;

   --  -------------------------------------------------------------------------

   function Unquote (Values : String) return String is
      use GNAT.Regpat;
      use Regexep;
      --  \[0-9]{1,3} match when \ is followed by 1 to 3 digits
      --  \u[0-9a-f]{4} match string starting with \u followed by 4 hex digits
      --  \. match \.
      --  In each case first to last refers to the characters follwing the /
      Pattern       : constant String := "\\([0-9]{1,3}|u[0-9a-f]{4}|.)";
      Matcher       : constant Pattern_Matcher := Compile (Pattern);
      Matches       : Matches_List;
      First         : Integer;
      Last          : Integer;
      Match_Found   : Boolean := False;
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

      return To_String (Result);

   end Unquote;

   --  -------------------------------------------------------------------------

   use Escape_Sub_Map_Package;
   Escape_Key : Unbounded_String;
   Escape_Val : Unbounded_String;
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
