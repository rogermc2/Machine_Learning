
with Ada.Text_IO; use Ada.Text_IO;

with ML_Types;

package body Load_ARFF_Data.Printing is

    procedure Print_ARFF (Data : ARFF_Record) is
    begin
        Print_Description (Data);
    end Print_ARFF;

    --  -------------------------------------------------------------------------

   procedure Print_Description (Data : ARFF_Record) is
      use ML_Types.Indefinite_String_Package;
      Header : constant ARFF_Header_Record := Data.Header;
      Info   : constant ARFF_Header := Header.Info;
      Curs   : Cursor := Info.First;
   begin
      Put_Line ("Dataset description:");
      while Has_Element (Curs) loop
         Put_Line (Element (Curs));
         Next (Curs);
      end loop;

    end Print_Description;

    --  -------------------------------------------------------------------------

end Load_ARFF_Data.Printing;
