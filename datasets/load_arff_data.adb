
with Ada.Streams.Stream_IO;
with Ada.Text_IO; use Ada.Text_IO;

with ML_Types;

package body Load_ARFF_Data is

    subtype ARFF_Header is ML_Types.String_List;

    procedure Load_ARFF_Header (File_ID : Ada.Streams.Stream_IO.File_Type;
                                Header  : out ARFF_Header);

    --  ------------------------------------------------------------------------

    procedure Load_ARFF (File_Name : String; Data : out ARFF_Record) is
        use Ada.Streams.Stream_IO;
        File_ID      : Ada.Streams.Stream_IO.File_Type;
        Data_Stream  : Ada.Streams.Stream_IO.Stream_Access;
        Header       : ARFF_Header;
    begin
        Open (File_ID, In_File, File_Name);
        Load_ARFF_Header (File_ID, Header);

        Close (File_ID);

    end Load_ARFF;

    --  ------------------------------------------------------------------------

    procedure Load_ARFF_Header (File_ID : Ada.Streams.Stream_IO.File_Type;
                                Header  : out ARFF_Header) is

    begin
        null;
    end Load_ARFF_Header;

    --  ------------------------------------------------------------------------

end Load_ARFF_Data;
