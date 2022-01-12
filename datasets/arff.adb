--  Based on scikit-learn/sklearn/externals _arff.py

with Ada.Assertions; use Ada.Assertions;
with Ada.Containers.Doubly_Linked_Lists;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
--  with Ada.Text_IO; use Ada.Text_IO;

with GNAT.Regpat;

with Dataset_Utilities;

pragma Warnings (Off);

package body ARFF is

    type TK_State is (TK_Descrition, TK_Comment, TK_Relation, TK_Attribute,
                      TK_Data);
    package String_Package is new Ada.Containers.Doubly_Linked_Lists (Unbounded_String);
    subtype String_List is String_Package.List;

    type Arff_Decoder is record
        Current_Line : Integer := 0;
    end record;

    procedure Decode_Comment
      (UC_Row : String; Arff_Container : in out JSON_Value);
    procedure Decode_Relation
      (UC_Row : String; Arff_Container : in out JSON_Value);

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
              (To_Unbounded_String (Fixed.Trim (Text (Pos1 .. Pos2 - 1), Both)));
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
            declare
                UC_Row : String :=
                           Dataset_Utilities.To_Upper_Case
                             (To_String (Element (Curs)));
            begin
                if UC_Row /= "" then
                    if UC_Row (1 .. 1) = "%" then
                        Decode_Comment (UC_Row, Arff_Container);
                    elsif UC_Row = "@RELATION" then
                        Assert (State = TK_Descrition, Routine_Name & Bad_Layout);
                        State := TK_Relation;
                        Decode_Relation (UC_Row, Arff_Container);
                    elsif UC_Row = "@ATTRIBUTE" then
                        State := TK_Attribute;
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

    procedure Decode_Comment (UC_Row         : String;
                              Arff_Container : in out JSON_Value) is
        use GNAT.Regpat;
        Matches : Match_Array (1 .. 2);
    begin
        Match (Compile ("^\%( )?"), UC_Row, Matches);
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
        Slices       : Array (1 .. 2) of Unbounded_String;
        Pos          : constant Integer := Fixed.Index (UC_Row, " ");
        Slice_1      : String := UC_Row (1 .. Pos - 1);
        Slice_2      : String :=
                         Fixed.Trim (UC_Row (Pos + 1 .. UC_Row'Length), Both);
        --        Matches : Match_Array (1 .. 2);
        S            : Membership;
        First        : Positive;
        Last         : Natural;
    begin
        --           Match (Compile ("^([^\{\}%,\s]*|"".*""|\'.*\')$"), Slice_2, Matches);
        Assert (Match (Compile ("^([^\{\}%,\s]*|"".*""|\'.*\')$"), Slice_2),
          Routine_Name & " relation declaration '" & Slice_2 &
          "' has an invalid format");

        Find_Token (To_Unbounded_String (Slice_2), """'", 1, Inside,
                    First, Last);

        --        Arff_Container.Set_Field ("description", Desc);

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

end ARFF;
