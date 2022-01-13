--  Based on scikit-learn/sklearn/externals _arff.py

with Ada.Assertions; use Ada.Assertions;
with Ada.Containers.Doubly_Linked_Lists;
with Ada.Strings.Fixed;
with Ada.Strings.Maps;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
--  with Ada.Text_IO; use Ada.Text_IO;

with GNAT.Regpat;

with Dataset_Utilities;

pragma Warnings (Off);

package body ARFF is

   type TK_State is (TK_Descrition, TK_Comment, TK_Relation, TK_Attribute,
                     TK_Data);
   package String_Package is new
     Ada.Containers.Doubly_Linked_Lists (Unbounded_String);
   subtype String_List is String_Package.List;

   type Arff_Decoder is record
      Conversers   : String_List;
      Current_Line : Integer := 0;
   end record;

   type Nominal_Conversor is record
      Values     : String_List;
      Zero_Value : Unbounded_String := To_Unbounded_String ("");
   end record;

   type Encoded_Nominal_Conversor is record
      Values : String_List;
   end record;

   procedure Decode_Attribute (Decoder         : in Out Arff_Decoder;
                               UC_Row          : String;
                               Encode_Nominal  : Boolean := False;
                               Attribute_Names : in out JSON_Value;
                               Arff_Container  : in out JSON_Value);
   procedure Decode_Comment
     (UC_Row : String; Arff_Container : in out JSON_Value);
   procedure Decode_Relation
     (UC_Row : String; Arff_Container : in out JSON_Value);
   function Parse_Values (UC_Row : Unbounded_String) return String_List;

   --  -------------------------------------------------------------------------

   function Decode (Decoder     : in Out Arff_Decoder;
                    Text        : String; Encode_Nominal : Boolean := False;
                    Return_Type : ARFF_Return_Type := Arff_Dense)
                    return JSON_Value is
      use Ada.Strings;
      use String_Package;
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
      Attribute_Names : JSON_Value := Create_Object;
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

      Curs := Message_Lines.First;
      while Has_Element (Curs) loop
         Decoder.Current_Line := Decoder.Current_Line + 1;
         declare
            UC_Row : String :=
                       Dataset_Utilities.To_Upper_Case
                         (To_String (Element (Curs)));
         begin
            if UC_Row /= "" then
               if UC_Row (1 .. 1) = "%" then
                  Decode_Comment (UC_Row, Arff_Container);
               elsif UC_Row = "@RELATION" then
                  Assert (State = TK_Descrition,
                          Routine_Name & Bad_Layout);
                  State := TK_Relation;
                  Decode_Relation (UC_Row, Arff_Container);
               elsif UC_Row = "@ATTRIBUTE" then
                  Assert (State = TK_Relation or
                            State = TK_Attribute,
                          Routine_Name & Bad_Layout);
                  State := TK_Attribute;
                  Decode_Attribute (Decoder, UC_Row, Encode_Nominal,
                                    Attribute_Names, Arff_Container);

               elsif UC_Row = "@DATA" then
                  State := TK_Data;
               end if;

            end if;

         end;
         Next (Curs);
      end loop;

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
      use Dataset_Utilities;
      Routine_Name : constant String := "ARFF.Decode_Relation ";
      Regex        : constant String :=
                       "^("".*""|'.*'|[^\{\}%,\s]*)\s+(.+)$";
      Trim_Seq     : constant Character_Sequence := "{} ";
      Trim_Set     : constant Character_Set := To_Set (Trim_Seq);
      Slices       : Array (1 .. 2) of Unbounded_String;
      --  L749 Extract raw name and type
      Pos          : Integer := Fixed.Index (UC_Row, " ");
      Slice_1      : constant String := UC_Row (1 .. Pos - 1);
      Slice_2      : String :=
                       Fixed.Trim (UC_Row (Pos + 1 .. UC_Row'Length), Both);
      Name         : Unbounded_String;
      Attr_Type    : Unbounded_String;
      Attribute    : JSON_Value := Create_Object;
      Values       : String_List;
      Converser    : JSON_Value;
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
         Values := Parse_Values (Attr_Type);
      else
         Slice_2 := Dataset_Utilities.To_Upper_Case (Slice_2);
         Assert (Slice_2 = "NUMERIC" or Slice_2 = "REAL" or
                   Slice_2 = "INTEGER" or Slice_2 = "STRING",
                 Routine_Name & " invalid attribute type, " & Slice_2);
      end if;

      declare
         Attr_Name : String := To_String (Name);
         Converser : Unbounded_String;
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
              null;
            else
              null;
            end if;

         else
            declare
               Converser_Map : JSON_Value := Create_Object;
               Lambda        : JSON_Value := Create_Object;
            begin
               Converser_Map.Set_Field ("STRING", "");
               Converser_Map.Set_Field ("INTEGER", Integer (0));
               Converser_Map.Set_Field ("NUMERIC", 0.0);
               Converser_Map.Set_Field ("Integer", 0.0);

               Converser := Get (Converser_Map, Get (Attribute, "name"));
            end;
         end if;
         Decoder.Conversers.Append (Converser);
      end;

   end Decode_Attribute;

   --  -------------------------------------------------------------------------

   procedure Decode_Comment (UC_Row         : String;
                             Arff_Container : in out JSON_Value) is
      use GNAT.Regpat;
      Regex   : constant String := "^\%( )?";
      Matches : Match_Array (1 .. 2);
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
      Slices       : Array (1 .. 2) of Unbounded_String;
      Pos          : Integer := Fixed.Index (UC_Row, " ");
      Slice_1      : constant String := UC_Row (1 .. Pos - 1);
      Slice_2      : String :=
                       Fixed.Trim (UC_Row (Pos + 1 .. UC_Row'Length), Both);
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

   procedure Decode_Rows_COO is

   begin
      null;
   end Decode_Rows_COO;

   --  -------------------------------------------------------------------------

   procedure Decode_Rows_Dense is

   begin
      null;
   end Decode_Rows_Dense;

   --  -------------------------------------------------------------------------

   function Get_Data_Object_For_Decoding (Matrix_Type : ARFF_Return_Type)
                                          return JSON_Value is
      Data_Object : JSON_Value;
   begin
      case Matrix_Type is
         when Arff_Coo => null;
         when Arff_Dense => null;
         when Arff_Dense_Gen => null;
         when Arff_Lod => null;
         when Arff_Lod_Gen => null;
      end case;

      return Data_Object;

   end Get_Data_Object_For_Decoding;

   --  -------------------------------------------------------------------------

   procedure Init_Encoded_Nominal_Conversor (Conversor : in out Encoded_Nominal_Conversor;
                                             Values    : String_List) is
   begin
      Conversor.Values := Values;

   end Init_Encoded_Nominal_Conversor;

   --  -------------------------------------------------------------------------

   procedure Init_Nominal_Conversor (Conversor : in out Nominal_Conversor;
                                     Values     : String_List) is
   begin
      Conversor.Values := Values;
      Conversor.Zero_Value := Values.First_Element;

   end Init_Nominal_Conversor;

   --  -------------------------------------------------------------------------

   procedure Init_Nominal_Conversor (Conversor : in out Nominal_Conversor;
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

   function Parse_Values (UC_Row : Unbounded_String) return String_List is
      use String_Package;
   begin
      return String_Package.Empty_List;

   end Parse_Values;

   --  -------------------------------------------------------------------------

end ARFF;
