
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;

with ML_Types;
with Printing;

package body Load_ARFF_Data.ARFF_Printing is

    --  -------------------------------------------------------------------------

    procedure Print_ARFF (Data : ARFF_Record) is
    begin
        Print_Description (Data);
        Put_Line ("Relation: " & Data.Header.Relation);
        Print_Attributes (Data);
        Print_Data (Data);
        New_Line;

    end Print_ARFF;

    --  -------------------------------------------------------------------------

    procedure Print_Attributes (Data : ARFF_Record) is
        use Ada.Strings;
        use Attribute_Data_Package;
        Header     : constant ARFF_Header_Record := Data.Header;
        Attributes : constant Attribute_List := Header.Attributes;
        Attribute  : Attribute_Record;
        Curs       : Cursor := Attributes.First;
    begin
        New_Line;
        Put_Line (Header.Relation & " dataset attributes:");
        while Has_Element (Curs) loop
            Attribute := Element (Curs);
            Put (Trim (Attribute.Name, Both) & " ");
            Ada.Text_IO.Unbounded_IO.Put_Line
              (Trim (To_Unbounded_String
               (ARFF_Data_Type'Image (Attribute.Data_Kind)), Both));
            if not Attribute.Nominal_Names.Is_Empty then
                Printing.Print_Indefinite_List
                  ("Nominal Names", Attribute.Nominal_Names);
            end if;
            Next (Curs);
        end loop;

    end Print_Attributes;

    --  -------------------------------------------------------------------------

    procedure Print_Data (Data : ARFF_Record) is
        use ARFF_Data_Package;
        Data_List : constant ARFF_Data_List := Data.Data;
        Curs      : Cursor := Data_List.First;
    begin
        New_Line;
        Put_Line ("Dataset data:");
        Put_Line ("Data length:" & Integer'Image (Integer (Data_List.Length)));
        while Has_Element (Curs) loop
            declare
                Data_Record : constant ARFF_Data_Record := Element (Curs);
            begin
                case Data_Record.Data_Kind is
                when ML_Types.Boolean_Type =>
                    Put_Line (Boolean'Image (Data_Record.Boolean_Data));
                when ML_Types.Float_Type =>
                    Put_Line (Float'Image (Data_Record.Real_Data));
                when ML_Types.Integer_Type =>
                    Put_Line (Integer'Image (Data_Record.Integer_Data));
                when ML_Types.UB_String_Type =>
                    Put_Line (Data_Record.UB_String_Data);
                end case;
            end;
            Next (Curs);
        end loop;

        New_Line;

    end Print_Data;

    --  -------------------------------------------------------------------------

    procedure Print_Description (Data : ARFF_Record) is
        use ML_Types.Indefinite_String_Package;
        Header : constant ARFF_Header_Record := Data.Header;
        Info   : constant ARFF_Header := Header.Info;
        Curs   : Cursor := Info.First;
    begin
        New_Line;
        Put_Line ("Dataset description:");
        while Has_Element (Curs) loop
            Put_Line (Element (Curs));
            Next (Curs);
        end loop;

        New_Line;

    end Print_Description;

    --  -------------------------------------------------------------------------

end Load_ARFF_Data.ARFF_Printing;
