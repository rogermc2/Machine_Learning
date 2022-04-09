--  Based on scikit-learn/sklearn/externals/_arff.py
with Ada.Assertions; use Ada.Assertions;
with Ada.Containers.Ordered_Maps;
with Ada.Integer_Text_IO;
with Ada.IO_Exceptions;
with Ada.Strings.Fixed;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Text_IO.Unbounded_IO;

with GNAT.Regpat;

with Maths;
with Utilities;

with Dataset_Utilities;
with Regexep;

--  with Load_ARFF_Data.ARFF_Printing;

package body Load_ARFF_Data is

   package Escape_Sub_Map_Package is new
     Ada.Containers.Ordered_Maps (Unbounded_String, Unbounded_String);

   Escape_Sub_Map : Escape_Sub_Map_Package.Map;
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
   --     Regex_CSV      : constant String := "[^,]+";
   --     Matcher_CSV    : constant GNAT.Regpat.Pattern_Matcher :=
   --                        GNAT.Regpat.Compile (Regex_CSV);
   --     Num_Parens_CSV : constant Natural := GNAT.Regpat.Paren_Count (Matcher_CSV);

   procedure Decode_Nominal (Attribute     : Attribute_Record;
                             Value_String  : String;
                             Decoded_Value : out Integer);
   procedure Load_Attributes (File_ID : File_Type;
                              aLine   : in out Unbounded_String;
                              Header  : in out ARFF_Header_Record);
   procedure Load_Data
     (File_ID : File_Type; aLine : in out Unbounded_String;
      Data    : in out ARFF_Record);
   procedure Load_Header (File_ID : File_Type; aLine : out Unbounded_String;
                          Header  : out ARFF_Header_Record);
   procedure Parse_Values (Row    : String;
                           Values : out NL_Types.Indef_String_List);
   function Split_Sparse_Line (Row : String) return NL_Types.Indef_String_List;
   procedure Swap (Data : in out NL_Types.Float_List_2D;
                   L, R : Positive);
   pragma Inline (Swap);
   function Unquote (Values : String) return String;

   --  ------------------------------------------------------------------------
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
         return Result;
      end;

   end Build_Re_Sparse;

   --  -------------------------------------------------------------------------
   --  L478
   function Decode_Dense_Values (Values     : NL_Types.Indef_String_List;
                                 Attributes : Attribute_List;
                                 Nominal_Value : out Integer)
                                 return AR_Real_List is
      use Ada.Containers;
      use Ada.Strings;
      use NL_Types;
      use Indefinite_String_Package;
      use Attribute_Data_Package;
      Routine_Name   : constant String := "Load_ARFF_Data.Decode_Dense_Values ";
      Attr_Cursor    : Attribute_Data_Package.Cursor;
      Values_Cursor  : Indefinite_String_Package.Cursor;
      ARFF_Data_Kind : ARFF_Data_Type;
      Attribute      : Attribute_Record;
      Decoded_Values : AR_Real_List;
   begin
      Assert (Values.Length = Attributes.Length, Routine_Name &
                "invalid data, number of values" &
                Count_Type'Image (Values.Length) &
                " should be the same as number of attributes" &
                Count_Type'Image (Attributes.Length));

      Attr_Cursor := Attributes.First;
      Values_Cursor := Values.First;

      while Has_Element (Attr_Cursor) loop
         Attribute := Element (Attr_Cursor);
         ARFF_Data_Kind := Attribute.Data_Kind;
         declare
            Value_String  : constant String := Element (Values_Cursor);
         begin
            case ARFF_Data_Kind is
               when ARFF_Integer | ARFF_Date | ARFF_Numeric | ARFF_String =>
                  null;

               when ARFF_Nominal =>
                  Decode_Nominal (Attribute, Value_String, Nominal_Value);

               when ARFF_Real =>
                  declare
                     Value : Float;
                  begin
                     if Fixed.Index (Value_String, ".") = 0 then
                        Value :=
                          Float (Integer'Value (Value_String));
                     else
                        Value := Float'Value (Value_String);
                     end if;
                     Decoded_Values.Append (Value);
                  end;
            end case;
         end;
         Next (Attr_Cursor);
         Next (Values_Cursor);
      end loop;

      return Decoded_Values;

   end Decode_Dense_Values;

   --  -------------------------------------------------------------------------

   procedure Decode_Nominal
     (Attribute     : Attribute_Record; Value_String : String;
      Decoded_Value : out Integer) is
      use Nominal_Data_Package;
      use Dataset_Utilities;
      Routine_Name    : constant String := "Load_ARFF_Data.Decode_Nominal ";
      UC_Value_String :  constant String := To_Upper_Case (Value_String);
      Nominal_Cursor  : Nominal_Data_Package.Cursor;
   begin
      if not Is_Empty (Attribute.Nominal_Data) then
         Nominal_Cursor := Attribute.Nominal_Data.First;
         while Has_Element (Nominal_Cursor) loop
            declare
               Nominal : constant Nominal_Data_Record := Element (Nominal_Cursor);
            begin
               if To_Upper_Case (Nominal.Data_String) = UC_Value_String then
                  Assert (Nominal.Data_Kind = Nominal_Integer, Routine_Name &
                            "Integer data expected.");
                  Decoded_Value := Nominal.Integer_Data;
               end if;
            end;
            Next (Nominal_Cursor);
         end loop;
      end if;

   end Decode_Nominal;

   --  -------------------------------------------------------------------------

   procedure Decode_Relation (aLine  : String;
                              Header : in out ARFF_Header_Record) is
      use Ada.Strings;
      use GNAT.Regpat;
      Routine_Name : constant String := "Load_ARFF_Data.Decode_Relation ";
      Regex        : constant String := "^([^\{\}%,\s]*|"".*""|'.*')$";
      Pos          : Integer := Fixed.Index (aLine, " ");
      Slice_2      : constant String :=
                       Fixed.Trim (aLine (Pos + 1 .. aLine'Last), Both);
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

      Header.Relation := UB_Slice;

   end Decode_Relation;

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

   function Get_Attributes (Data : ARFF_Record) return Attribute_List is
   begin
      return Data.Header.Attributes;

   end Get_Attributes;

   --  -------------------------------------------------------------------------

   function Get_Description (Data : ARFF_Record) return ARFF_Header is
   begin
      return Data.Header.Info;

   end Get_Description;

   --  -------------------------------------------------------------------------

   function Get_Relation (Data : ARFF_Record) return String is
   begin
      return To_String (Data.Header.Relation);

   end Get_Relation;

   --  -------------------------------------------------------------------------

   procedure Load_ARFF (File_Name : String; Data : out ARFF_Record) is
      Routine_Name : constant String := "Load_ARFF_Data.Load_ARFF ";
      File_ID      : File_Type;
      aLine        : Unbounded_String;
   begin
      Open (File_ID, In_File, File_Name);
      Put_Line (Routine_Name & File_Name & " opened");
      --  L798
      Load_Header (File_ID, aLine, Data.Header);
      Put_Line (Routine_Name & "Header loaded");
      New_Line;
      --  L873 obj['data'] = data.decode_rows
      Load_Data (File_ID, aLine, Data);
      Put_Line (Routine_Name & "data loaded");
      Close (File_ID);
      pragma Unreferenced (File_ID);
      pragma Unreferenced (aLine);

   exception
      when Ada.IO_Exceptions.Name_Error  =>
         Put_Line (Routine_Name & "can't find the file " & File_Name & "!");
         raise;
      when others =>
         Put_Line ("An exception occurred in " & Routine_Name);
         raise;

   end Load_ARFF;

   --  ------------------------------------------------------------------------

   procedure Load_Attributes (File_ID : File_Type;
                              aLine   : in out Unbounded_String;
                              Header  : in out ARFF_Header_Record) is
      use Ada.Strings;
      use Unbounded_IO;
      Routine_Name    : constant String := "Load_ARFF_Data.Load_Attributes ";
      H_Tab           : String (1 .. 1);
      Pos_1           : Positive;
      Pos_2           : Natural;
      EOL             : Boolean := False;
      Data_Kind       : Unbounded_String;
      Attribute       : Attribute_Record;
      Nominal_Text    : Unbounded_String;
      Nominal_ML_Type : NL_Types.Data_Type;
      Nominal_Kind    : Nominal_Data_Type;
   begin
      H_Tab (1) := ASCII.HT;
      while Length (aLine) = 0 or else Slice (aLine, 1, 1) /= "@" loop
         aLine := Get_Line (File_ID);
      end loop;

      while Length (aLine) > 0 and then  Slice (aLine, 1, 1) = "@" loop
         Assert (Slice (aLine, 1, 10) = "@ATTRIBUTE", Routine_Name &
                   "invalid ARFF format, " & To_String (aLine) & " but " &
                   " line beginning @ATTRIBUTE expected");
         Pos_1 := 12;
         Pos_2 := Fixed.Index (Slice (aLine, Pos_1, Length (aLine)), " ");
         if Pos_2 = 0 then
            Pos_2 := Fixed.Index (Slice (aLine, Pos_1, Length (aLine)), H_Tab);
         end if;

         Attribute.Name := Trim (To_Unbounded_String
                                 (Slice (aLine, Pos_1, Pos_2)), Both);
         if Element (Attribute.Name, Length (Attribute.Name)) = ASCII.HT then
            Attribute.Name := To_Unbounded_String
              (Slice (Attribute.Name, 1, Length (Attribute.Name) - 1));
         end if;

         Pos_1 := Pos_2;
         while Element (aLine, Pos_1) = ' ' or else
           Element (aLine, Pos_1) = ASCII.HT loop
            Pos_1 := Pos_1 + 1;
         end loop;

         Data_Kind := Trim (To_Unbounded_String
                            (Slice (aLine, Pos_1, Length (aLine))), Right);
         Data_Kind := Dataset_Utilities.To_Upper_Case (Data_Kind);
         if Data_Kind = To_Unbounded_String ("REAL") then
            Attribute.Data_Kind := ARFF_Real;
         elsif Data_Kind = To_Unbounded_String ("INTEGER") then
            Attribute.Data_Kind := ARFF_Integer;
         elsif Data_Kind = To_Unbounded_String ("DATE") then
            Attribute.Data_Kind := ARFF_Date;
         elsif Data_Kind = To_Unbounded_String ("STRING") then
            Attribute.Data_Kind := ARFF_String;
         elsif Slice (Data_Kind, 1, 1) = "{" then
            Attribute.Data_Kind := ARFF_Nominal;
            Pos_1 := Pos_2;
            while Element (aLine, Pos_1) = ' ' or else
              Element (aLine, Pos_1) = ASCII.HT loop
               Pos_1 := Pos_1 + 1;
            end loop;
            Pos_1 := Pos_1 + 1;

            while not EOL loop
               Pos_2 := Fixed.Index
                 (Slice (aLine, Pos_1, Length (aLine)), ",");
               EOL := Pos_2 = 0;
               if EOL then
                  Pos_2 := Fixed.Index
                    (Slice (aLine, Pos_1, Length (aLine)), "}");
               end if;

               Nominal_Text :=
                 To_Unbounded_String (Slice (aLine, Pos_1, Pos_2 - 1));
               --                 Put_Line (Routine_Name & "Nominal_Text: " & Nominal_Text);
               Nominal_ML_Type := Utilities.Get_Data_Type (Nominal_Text);
               case Nominal_ML_Type is
               when NL_Types.Integer_Type =>
                  Nominal_Kind := Nominal_Integer;
               when NL_Types.Float_Type =>
                  Nominal_Kind := Nominal_Real;
               when NL_Types.Boolean_Type | NL_Types.UB_String_Type =>
                  Nominal_Kind := Nominal_String;
               end case;

               declare
                  Nominal : Nominal_Data_Record (Nominal_Kind);
               begin
                  Nominal.Data_String := Nominal_Text;
                  case Nominal_Kind is
                  when Nominal_Integer =>
                     Nominal.Integer_Data :=
                       Integer'Value (To_String (Nominal_Text));
                  when Nominal_Numeric | Nominal_Real =>
                     Nominal.Real_Data :=
                       Float'Value (To_String (Nominal_Text));
                  when Nominal_String =>
                     Nominal.UB_String_Data := Nominal_Text;
                  end case;
                  Attribute.Nominal_Data.Append (Nominal);
               end;

               Pos_1 := Pos_2 + 1;
            end loop;
         else
            Put_Line (Routine_Name & "unknown attribute data type: " &
                        Data_Kind);
         end if;

         Header.Attributes.Append (Attribute);
         aLine := Get_Line (File_ID);
      end loop;

   end Load_Attributes;

   --  ------------------------------------------------------------------------
   --  Based on L461 decode rows
   procedure Load_Data
     (File_ID : File_Type; aLine : in out Unbounded_String;
      Data    : in out ARFF_Record) is
      use Unbounded_IO;
      Routine_Name : constant String := "Load_ARFF_Data.Load_Data ";
      Attributes   : constant Attribute_List := Data.Header.Attributes;
      Values       : NL_Types.Indef_String_List;
      Target_List  : AR_Integer_List;
      Target_Value : Integer;
   begin
      Put_Line (Routine_Name & "loading data");
      while Length (aLine) = 0 or else Slice (aLine, 1, 1) /= "@" loop
         aLine := Get_Line (File_ID);
      end loop;
      Assert (Slice (aLine, 1, 5) = "@DATA", Routine_Name &
                "invalid ARFF format, " & To_String (aLine) & " but " &
                " line beginning @DATA expected");
      --  L461 decode rows
      while not End_Of_File (File_ID) loop
         aLine := Get_Line (File_ID);
         Target_List.Clear;
         if Length (aLine) > 0 and then Element (aLine, 1) /= '%' then
            Parse_Values (To_String (aLine), Values);
            Data.Data.Append (Decode_Dense_Values
                              (Values, Attributes, Target_Value));
            Target_List.Append (Target_Value);
            Data.Target.Append (Target_List);
         end if;
      end loop;

   end Load_Data;

   --  ------------------------------------------------------------------------

   procedure Load_Header (File_ID : File_Type;
                          aLine   : out Unbounded_String;
                          Header  : out ARFF_Header_Record) is
      use Unbounded_IO;
      Routine_Name : constant String := "Load_ARFF_Data.Load_ARFF_Header ";
      Is_Info      : Boolean := True;
   begin
      while Is_Info loop
         aLine := Get_Line (File_ID);
         if aLine /= "" then
            Is_Info := Slice (aLine, 1, 1) = "%";
            if Is_Info then
               Header.Info.Append (Slice (aLine, 2, Length (aLine)));
            end if;
         end if;
      end loop;

      while aLine = "" or else Slice (aLine, 1, 1) /= "@" loop
         aLine := Get_Line (File_ID);
      end loop;

      Assert (Length (aLine) > 10 and then Slice (aLine, 1, 9) = "@RELATION", Routine_Name &
                "line commencing @RELATION expected but received " &
                To_String (aLine));
      Decode_Relation (To_String (aLine), Header);
      aLine := Get_Line (File_ID);

      Load_Attributes (File_ID, aLine, Header);

   end Load_Header;

   --  ------------------------------------------------------------------------
   --  L283 Parse_Values splits a line into a list of values
   --  Match produces Matches of type Match_Array.
   --  Each component of Matches is set to the subrange of the
   --  matches substring or to No_Match if no match.
   --  Matches (N) is for the  N'th parenthesized subexpressions;
   --  Matches (0) is for the whole expression.
   procedure Parse_Values (Row    : String;
                           Values : out NL_Types.Indef_String_List) is
      use GNAT.Regpat;
      use Regexep;
      use NL_Types;
      use Indefinite_String_Package;
      use String_Package;
      Routine_Name        : constant String := "Load_ARFF_Data.Parse_Values ";
      Non_Trivial         : constant String := "[""\'{}\\s]";
      Non_Trivial_Matcher : constant Pattern_Matcher := Compile (Non_Trivial);
      First               : Positive;
      Last                : Positive;
      Match_Found         : Boolean;
      Dense_Match         : Boolean;
      Sparse_Match        : Boolean;
      Matches             : Matches_List;
      Errors              : String_List;
      Errors_Cursor       : String_Package.Cursor;
   begin
      if Row'Length /= 0 and then Row /= "?" then
         Matches := Find_Match (Non_Trivial_Matcher, Row, First, Last,
                                Match_Found);
         pragma Unreferenced (Matches);
         if Match_Found then
            --  not nontrivial
            --  Row contains none of the Non_Trivial characters
            Values := Dataset_Utilities.Get_CSV_Data (Row);
         else
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
                  Values := Dataset_Utilities.Get_CSV_Data
                    (Row (First .. Last));
               else
                  Matches := Find_Match (Sparse_Matcher, Row, First, Last,
                                         Sparse_Match);
                  if Sparse_Match then
                     Put_Line (Routine_Name & "Sparse_Match");
                     Errors := Dataset_Utilities.Split_String
                       (Row (First .. Last), ",");
                     if not Errors.Is_Empty then
                        Errors_Cursor := Errors.First;
                        while Has_Element (Errors_Cursor) loop
                           Values.Append
                             (Unquote (To_String (Element (Errors_Cursor))));
                           Next (Errors_Cursor);
                        end loop;

                     else
                        Values := Split_Sparse_Line (Row);
                     end if;
                  end if;

               end if;
            end;  --  declare block

         end if;
      end if;

   end Parse_Values;

   --  -------------------------------------------------------------------------

   function Permute (aList : NL_Types.Float_List_2D)
                     return NL_Types.Float_List_2D is
      List_Length  : constant Positive := Positive (aList.Length);
      Rand         : Positive;
      Permutation  : NL_Types.Float_List_2D := aList;
   begin
      if List_Length > 1 then
         for index in 1 .. List_Length - 1 loop
            Rand := index +
              Natural (abs (Maths.Random_Float) * Float (List_Length - index));
            Swap (Permutation, index, Rand);
         end loop;
      end if;

      return Permutation;

   end Permute;

   --  -------------------------------------------------------------------------

   function Split_Sparse_Line (Row : String)
                            return NL_Types.Indef_String_List is
      use GNAT.Regpat;
      use Regexep;
      use Matches_Package;
      use NL_Types;
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

   procedure Swap (Data : in out NL_Types.Float_List_2D;
                   L, R : Positive) is
      Item : NL_Types.Float_List;
   begin
      Item := Data.Element (L);
      Data.Replace_Element (L, Data.Element (R));
      Data.Replace_Element (R, Item);
   end Swap;

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

end Load_ARFF_Data;