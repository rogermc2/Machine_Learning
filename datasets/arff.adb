--  Based on scikit-learn/sklearn/externals _arff.py

with Ada.Containers.Doubly_Linked_Lists;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
--  with Ada.Text_IO; use Ada.Text_IO;

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

    function Decode (Decoder : in Out Arff_Decoder; Text : String; Encode_Nominal : Boolean := False;
                     Return_Type : ARFF_Return_Type := Arff_Dense)
                     return JSON_Value is
        use Ada.Strings;
        use String_Package;
        Text_Length         : constant Integer := Text'Length;
        State               : TK_State := TK_Descrition;
        Pos1                : Integer := 1;
        Pos2                : Integer := 1;
        Message_Lines       : String_List;
        Curs                : Cursor;
        Arff_Container_Type : JSON_Value := Create_Object;
        ARFF_Data           : JSON_Value;
        Attribute_Names     : JSON_Value := Create_Object;
    begin
        Decoder.Current_Line := 0;
        while Pos2 /= 0 and Pos1 < Text_Length loop
            Pos2 := Fixed.Index (Text, "\r\n");
            Message_Lines.Append
              (To_Unbounded_String (Text (Pos1 .. Pos2 - 1)));
            Pos1 := Pos2 + 3;
        end loop;
        if Pos1 < Text_Length - 4 then
            Message_Lines.Append
              (To_Unbounded_String (Text (Pos1 .. Text_Length - 4)));
        end if;

        Arff_Container_Type.Set_Field ("description", "");
        Arff_Container_Type.Set_Field ("relation", "");
        Arff_Container_Type.Set_Field ("attributes", Create (Empty_Array));
        Arff_Container_Type.Set_Field ("description", Create (Empty_Array));

        Curs := Message_Lines.First;
        while Has_Element (Curs) loop
            declare
                Row : String := To_String (Element (Curs));
            begin
                if Row /= "" then
                    Row := Dataset_Utilities.To_Upper_Case (Row);
                    if Row (1 .. 1) = "%" then
                        null;
                    end if;
                end if;
            end;
            Next (Curs);
        end loop;

        return ARFF_Data;

    end Decode;

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

    function Load (File_Data : String; Encode_Nominal : Boolean := False;
                   Return_Type : ARFF_Return_Type := Arff_Dense)
                   return JSON_Value is
        Decoder : Arff_Decoder;
        ARFF_Data : constant JSON_Value :=
                      Decode (Decoder, File_Data, Encode_Nominal, Return_Type);
    begin
        return ARFF_Data;

    end Load;

    --  -------------------------------------------------------------------------

end ARFF;
