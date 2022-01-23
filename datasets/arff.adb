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

--  pragma Warnings (Off);

package body ARFF is

   type TK_State is (TK_Descrition, TK_Relation, TK_Attribute);
   --                       TK_Comment, TK_Data);
   type Conversor_Type is (Conversor_Unencoded, Conversor_Encoded,
                           Conversor_Map);
   type Conversor_Data_Type is (Conv_Integer, Conv_Numeric, Conv_Real,
                                Conv_String);

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

   --     type Converser_Map is record
   --        Text    : Unbounded_String := To_Unbounded_String ("");
   --        Int     : Integer := 0;
   --        Numeric : Float := 0.0;
   --        Real    : Float := 0.0;
   --     end record;

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

   type Conversor_Item is record
      Name      : Unbounded_String;
      Data_Type : Conversor_Data_Type;
   end record;

   package Conversor_Item_Package is new
     Ada.Containers.Doubly_Linked_Lists (Conversor_Item);
   subtype Conversor_Item_List is Conversor_Item_Package.List;

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

   function Decode_Attribute (Decoder           : in Out Arff_Decoder;
                              UC_Row            : String;
                              Attribute_Names   : in out JSON_Value;
                              Encode_Nominal    : Boolean := False)
                              return JSON_Value;
   procedure Decode_Comment
     (UC_Row : String; Arff_Container : in out JSON_Value);
   procedure Decode_Relation
     (UC_Row : String; Arff_Container : in out JSON_Value);
   --     function Decode_COO_Rows
   --       (Decoder : in Out Arff_Decoder; Stream_Func : Stream_Func_Type;
   --        Data    : out Classifier_Types.Float_List; Rows, Cols : out Integer_List)
   --        return String_List;
   function Decode_Dense_Rows (Decoder     : in Out Arff_Decoder;
                               Stream_Func : Stream_Func_Type)
                               return JSON_Array;
   function Decode_Dense_Values (Values : String_List; Conversers : Conversor_List)
                                 return JSON_Array;
   function Max_Value (Values : String_List) return Integer;
   function Parse_Values (Row : String) return String_List;
   procedure Process_JSON_Array (Decoder        : in out Arff_Decoder;
                                 Attribute      : JSON_Value;
                                 Encode_Nominal : Boolean);
   function Sparse_Line (Row : String) return Boolean;
   function Stream_Data (Decoder : in out Arff_Decoder) return String;
   function Unquote (Values : String) return Unbounded_String;

   --  -------------------------------------------------------------------------

   procedure ARFF_Syntax_Error (Row : String) is
   begin
      raise ARFF_Error with "ARFF unknown parsing error";

   end ARFF_Syntax_Error;

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
   --  L771  and, by reference, L879
   function Decode_ARFF (Decoder     : in Out Arff_Decoder;
                         Text           : String;
                         Encode_Nominal : Boolean := False;
                         Matrix_Type : ARFF_Return_Type := Arff_Dense)
                         return JSON_Value is
      use Ada.Strings;
      use ML_Types.String_Package;
      Routine_Name    : constant String := "ARFF.Decode_ARFF ";
      Bad_Layout      : constant String := " layout of ARFF file is bad.";
      Text_Length     : constant Integer := Text'Length;
      State           : TK_State := TK_Descrition;
      Pos1            : Integer := 1;
      Pos2            : Integer := 1;
      Message_Lines   : String_List;
      Curs            : Cursor;
      Arff_Container  : Arff_Container_Type := Create_Object;
      Attr            : JSON_Value := Create_Object;
      Attribute_Names : JSON_Value := Create_Object;
      Stream_Row      : Unbounded_String;
      Values          : JSON_Array;
      JSON_Values     : JSON_Array;
      Attribute_Array : JSON_Array;
--        Data            : JSON_Value;
--        Rows, Cols      : Integer_List;
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

      --  L784 Arff_Container implements obj: ArffContainerType
      Arff_Container.Set_Field ("description", "");
      Arff_Container.Set_Field ("relation", "");
      Arff_Container.Set_Field ("attributes", Create (Empty_Array));
      Arff_Container.Set_Field ("description", Create (Empty_Array));

      --  L793  Create the data helper object
      --        Data := Get_Data_Object_For_Decoding (Matrix_Type);

      --  L796  Read all lines
      Curs := Message_Lines.First;
      while Has_Element (Curs) loop
         Decoder.Current_Line := Decoder.Current_Line + 1;
         declare
            UC_Row : constant String :=
                       Dataset_Utilities.To_Upper_Case
                         (To_String (Element (Curs)));
         begin
            if UC_Row /= "" then
               if UC_Row (1 .. 9) = "@RELATION" then
                  Assert (State = TK_Descrition,
                          Routine_Name & Bad_Layout);
                  State := TK_Relation;
                  Decode_Relation (UC_Row, Arff_Container);

                  --  L821 _TK_ATTRIBUTE = "@ATTRIBUTE"
               elsif UC_Row (1 .. 10) = "@ATTRIBUTE" then
                  Assert (State = TK_Relation or State = TK_Attribute,
                          Routine_Name & Bad_Layout);
                  State := TK_Attribute;
                  Attr := Decode_Attribute (Decoder, UC_Row, Attribute_Names,
                                            Encode_Nominal);
                  --  _decode lines 827 - 830 (update attribute_names) are
                  --  implemented in Decode_Attribute
                  --  L832
                  Attribute_Array := Get (Arff_Container, "attributes");
                  Append (Attribute_Array, Attr);
                  Arff_Container.Set_Field ("attributes", Attribute_Array);

               elsif UC_Row (1 .. 5) = "@DATA" then
                  --  L850
                  Assert (State = TK_Attribute, Routine_Name & Bad_Layout);

               elsif UC_Row (1 .. 1) = "%" then
                  --  L806
                  Decode_Comment (UC_Row, Arff_Container);

               else
                  Assert (False, Routine_Name & Bad_Layout);
               end if;
            end if;
         end;

         Next (Curs);
      end loop;

      --  L872 Alter the data object
      Stream_Cursor := Message_Lines.First;
      --  case Matrix_Type implements
      --  L792 data = _get_data_object_for_decoding(matrix_type)
      case Matrix_Type is
         when Arff_Dense =>
            Values := Decode_Dense_Rows (Decoder, Stream_Data'Access);
         when Arff_Coo => null;
            --              Values := Decode_COO_Rows (Decoder, Stream_Data'Access, Data,
            --                                         Rows, Cols);
         when others => null;
      end case;

--        Curs := Values.First;
--        while Has_Element (Curs) loop
--           Append (JSON_Values, Create (Element (Curs)));
--           Next (Curs);
--        end loop;
      Arff_Container.Set_Field ("data", JSON_Values);

      Stream_Row := Get (Arff_Container, "description");
      if Tail (Stream_Row, 2) = "\n" then
         Stream_Row := To_Unbounded_String
           (Slice (Stream_Row, 1, Length (Stream_Row) - 2));
         Arff_Container.Set_Field ("description", Stream_Row);
      end if;

      return Arff_Container;

   end Decode_ARFF;

   --  -------------------------------------------------------------------------

   function Decode_Attribute
     (Decoder         : in Out Arff_Decoder; UC_Row : String;
      Attribute_Names : in out String_List;
      Encode_Nominal  : Boolean := False) return Conversor_Item is
      use GNAT.Regpat;
      use ML_Types.String_Package;
      use Ada.Strings;
      use Ada.Strings.Maps;
      Routine_Name    : constant String := "ARFF.Decode_Relation ";
      Regex           : constant String :=
                          "^("".*""|'.*'|[^\{\}%,\s]*)\s+(.+)$";
      Trim_Seq        : constant Character_Sequence := "{} ";
      Trim_Set        : constant Character_Set := To_Set (Trim_Seq);
      --  L749 Extract raw name and type
      Pos             : Integer := Fixed.Index (UC_Row, " ");
      Slice_1         : constant String := UC_Row (UC_Row'First .. Pos - 1);
      Slice_2         : String :=
                          Fixed.Trim (UC_Row (Pos + 1 .. UC_Row'Last), Both);
      Name            : Unbounded_String;
      Attr_Type       : Unbounded_String;
      Values          : String_List;
      Conv_Item       : Conversor_Item;
   begin
      Name := To_Unbounded_String (Slice_1);
      Assert (Match (Compile (Regex), Slice_2),
              Routine_Name & " attribute declaration '" &
                To_String (Attr_Type) &
                "' has an invalid format.");

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
      --  end Python _arff._decode_attribute

      Conv_Item.Name := Name;
      if Slice_2 = "INTEGER" then
            Conv_Item.Data_Type := Conv_Integer;
      elsif Slice_2 = "NUMERIC" then
            Conv_Item.Data_Type := Conv_Numeric;
      elsif Slice_2 = "REAL" then
            Conv_Item.Data_Type := Conv_Real;
      elsif Slice_2 = "STRING" then
            Conv_Item.Data_Type := Conv_String;
      end if;

      return Conv_Item;

   end Decode_Attribute;

   --  -------------------------------------------------------------------------

   function Decode_Attribute
     (Decoder         : in Out Arff_Decoder; UC_Row : String;
      Attribute_Names : in out JSON_Value;
      Encode_Nominal  : Boolean := False) return JSON_Value is
      use Ada.Strings;
      use Ada.Strings.Maps;
      use GNAT.Regpat;
      use ML_Types.String_Package;
      Routine_Name    : constant String := "ARFF.Decode_Relation ";
      Regex           : constant String :=
                          "^("".*""|'.*'|[^\{\}%,\s]*)\s+(.+)$";
      Trim_Seq        : constant Character_Sequence := "{} ";
      Trim_Set        : constant Character_Set := To_Set (Trim_Seq);
      Arff_Container  : JSON_Value;
      --  L749 Extract raw name and type
      Pos             : Integer := Fixed.Index (UC_Row, " ");
      Slice_1         : constant String := UC_Row (UC_Row'First .. Pos - 1);
      Slice_2         : String :=
                          Fixed.Trim (UC_Row (Pos + 1 .. UC_Row'Last), Both);
      Name            : Unbounded_String;
      Attr_Type       : Unbounded_String;
      Attribute       : constant JSON_Value := Create_Object;
      Curs            : Cursor;
      Values          : String_List;
      JSON_Values     : JSON_Array;
   begin
      Name := To_Unbounded_String (Slice_1);
      Assert (Match (Compile (Regex), Slice_2),
              Routine_Name & " attribute declaration '" &
                To_String (Attr_Type) &
                "' has an invalid format.");

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
      --  end Python _arff._decode_attribute

      Curs := Values.First;
      while Has_Element (Curs) loop
         Append (JSON_Values, Create (Element (Curs)));
         Next (Curs);
      end loop;

      --  L827  Originally in ATTRIBUTE section of _decode
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
            Process_JSON_Array (Decoder, Attribute, Encode_Nominal);

         else
            declare
               Converser     : Conversor_Data (Conversor_Encoded);
               Converser_Map : constant JSON_Value := Create_Object;
               --                      Lambda        : JSON_Value := Create_Object;
            begin
               --  L838
               Converser_Map.Set_Field ("STRING", "");
               Converser_Map.Set_Field ("INTEGER", Integer (0));
               Converser_Map.Set_Field ("NUMERIC", 0.0);
               Converser_Map.Set_Field ("REAL", 0.0);
               declare
                  data : constant UTF8_String := Converser_Map.Get ("INTEGER");
               begin
                  Converser.Encoded_Values.Append (To_Unbounded_String (Data));
               end;
               Decoder.Conversers.Append (Converser);
            end;
         end if;
      end;

      return Attribute;

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
   function Decode_Dense_Rows (Decoder     : in Out Arff_Decoder;
                               Stream_Func : Stream_Func_Type)
                               return JSON_Array is
      use String_Package;
      Routine_Name     : constant String := "ARFF.Decode_Rows_Dense ";
      Converser_Length : constant Natural := Natural (Decoder.Conversers.Length);
      --  L462  for row in stream:
      Row              : constant String := Stream_Func (Decoder);
      --  L463
      Values           : constant String_List := Parse_Values (Row);
   begin
      Assert (not Values.Is_Empty and Max_Value (Values) < Converser_Length ,
              Routine_Name & Row & " format is invalid.");
      --  L475
      return Decode_Dense_Values (Values, Decoder.Conversers);

   end Decode_Dense_Rows;

   --  -------------------------------------------------------------------------
   --  L478
   function Decode_Dense_Values
     (Values : String_List; Conversers : Conversor_List) return JSON_Array is
      use Ada.Containers;
      use Conversor_Package;
      use String_Package;
      use Conversor_Tuple_Package;

      Routine_Name         : constant String := "ARFF.Decode_Values ";
      Zipped_Values        : Conversor_Tuple_List;
      Conv_Cursor          : Conversor_Package.Cursor;
      Values_Cursor        : String_Package.Cursor;
--        Zip_Cursor     : Conversor_Tuple_Package.Cursor;
      aConverser           : Conversor_Data (Conversor_Encoded);
      Converser_Values     : ML_Types.String_List;
      Converser_Zero_Value : Unbounded_String;
      Converser_String     : Unbounded_String;
      Value_String         : Unbounded_String;
      Decoded_Values       : JSON_Array;
   begin
      Assert (Values.Length = Conversers.Length, Routine_Name &
                "lengths of Values " & Count_Type'Image (Values.Length) &
                " and Conversers" & Count_Type'Image (Conversers.Length) &
                " are diferrent.");

      Conv_Cursor := Conversers.First;
      Values_Cursor := Values.First;

      while Has_Element (Conv_Cursor) loop
         Zipped_Values.Append
           ((Element (Conv_Cursor), Element (Values_Cursor)));
         aConverser := Element (Conv_Cursor);
         Converser_String := aConverser.Encoded_Values;
         Value_String := Element (Values_Cursor);
         Next (Conv_Cursor);
         Next (Values_Cursor);
      end loop;

--        Zip_Cursor := Zipped_Values.First;
--        while Has_Element (Zip_Cursor) loop
--           aValue :=
--           Decoded_Values.Append (Element (Zip_Cursor).Value);
--           Next (Zip_Cursor);
--        end loop;

      return Decoded_Values;

   end Decode_Dense_Values;

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
         Next (Curs);
      end loop;

      return Max;

   end Max_Value;

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
                    Decode_ARFF (Decoder, File_Data, Encode_Nominal,
                                 Return_Type);
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
      Value_Cursor : Cursor;
      Errors       : String_List;
      Result       : String_List;
   begin
      if Row'Length /= 0 and then Row /= "?" then
         Matches := Find_Match (Compile (Non_Trivial), Row, First, Last,
                                Match_Found);
         pragma Unreferenced (Matches);
         if Match_Found then
            --  not nontrivial
            --  Row contains none of the Non_Trivial characters
            Values := Get_CSV_Data (Row);
         else
            --  Row contains Non_Trivial characters
            --  Build_Re_Dense and Build_Re_Sparse (_RE_DENSE_VALUES) tokenize
            --  despite quoting, whitespace, etc.
            declare
               Dense  : constant Pattern_Matcher := Build_Re_Dense;
               Sparse : constant Pattern_Matcher := Build_Re_Sparse;
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
               Value_Cursor := Values.First;
               while Has_Element (Value_Cursor) loop
                  Result.Append (Unquote (To_String (Element (Value_Cursor))));
                  Next (Value_Cursor);
               end loop;

            elsif not Sparse_Line (Row) then
               ARFF_Syntax_Error (Row);
            end if;
         end if;
      end if;

      return Values;

   end Parse_Values;

   --  -------------------------------------------------------------------------

   procedure Process_JSON_Array (Decoder        : in out Arff_Decoder;
                                 Attribute      : JSON_Value;
                                 Encode_Nominal : Boolean) is
   begin
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

   end Process_JSON_Array;

   --  -------------------------------------------------------------------------

   function Sparse_Line (Row : String) return Boolean is
      use GNAT.Regpat;
      use Regexep;
      use String_Package;
      Regex       : constant String := "^\s*\{.*\}\s*$";
      Matcher     : constant Pattern_Matcher := Build_Re_Sparse;
      First       : Positive;
      Last        : Positive;
      Matches     : Matches_List;
      Match_Found : Boolean;
      Result      : Boolean := False;
   begin
      Matches := Find_Match (Matcher, Row, First, Last, Match_Found);
      if Match_Found then
         null;
      end if;

      return Result;

   end Sparse_Line;

   --  -------------------------------------------------------------------------

   function Stream_Data (Decoder : in out Arff_Decoder) return String is
      use Ada.Strings;
      use String_Package;
      Row    : Unbounded_String;
      Result : Unbounded_String := To_Unbounded_String ("");
   begin
      while Has_Element (Stream_Cursor) and then Length (Row) > 0 loop
         Decoder.Current_Line := Decoder.Current_Line + 1;
         Row := Element (Stream_Cursor);
         Trim (Row, Both);
         declare
            UC_Row : constant String := Dataset_Utilities.To_Upper_Case
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
