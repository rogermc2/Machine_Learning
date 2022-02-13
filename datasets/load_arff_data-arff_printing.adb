
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
      Put_Line (Header.Relation & " dataset description:");
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

      New_Line;

    end Print_Attributes;

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
