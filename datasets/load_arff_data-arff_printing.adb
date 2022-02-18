
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;

with ML_Types;
--  with Printing;

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
      Curs       : Attribute_Data_Package.Cursor := Attributes.First;
   begin
      New_Line;
      Put_Line ("Dataset attributes:");
      while Has_Element (Curs) loop
         Attribute := Element (Curs);
         Put (Trim (Attribute.Name, Both) & " ");
         Ada.Text_IO.Unbounded_IO.Put_Line
           (Trim (To_Unbounded_String
            (ARFF_Data_Type'Image (Attribute.Data_Kind)), Both));
         if not Attribute.Nominal_Data.Is_Empty then
            New_Line;
            Put_Line ("*** Has nominal data ***");
            New_Line;
--              Printing.Print_Indefinite_List
--                ("Nominal Names", Attribute.Nominal_Names);
--           if not Attribute.Nominal_Names.Is_Empty then
--              Printing.Print_Indefinite_List
--                ("Nominal Names", Attribute.Nominal_Names);
         end if;
         Next (Curs);
      end loop;

   end Print_Attributes;

   --  -------------------------------------------------------------------------

   procedure Print_Data (Data : ARFF_Record; Start : Positive := 1;
                         Last : Positive := 10) is
      use ARFF_Data_List_Package;
      Data_List_2D : constant ARFF_Data_List_2D := Data.Data;
      List_Curs    : ARFF_Data_List_Package.Cursor := Data_List_2D.First;
      Data_List    : ARFF_Data_List;
      Data_Curs    : ARFF_Data_Package.Cursor;
      Count        : Natural := Start - 1;
   begin
      New_Line;
      Put_Line ("Dataset data:");
      Put_Line ("Data length:" & Integer'Image (Integer (Data_List_2D.Length)));
      while Has_Element (List_Curs) and Count <= Last loop
         Count := Count + 1;
         Data_List := Element (List_Curs);

         if Count >= Start and then Count <= Last then
            Put_Line ("Data row:" & Integer'Image (Count));
            Data_Curs := Data_List.First;
            while Has_Element (Data_Curs) loop
               declare
                  Data_Record : constant ARFF_Data_Record := Element (Data_Curs);
               begin
                  case Data_Record.Data_Kind is
                  when ML_Types.Boolean_Type =>
                     Put (Boolean'Image (Data_Record.Boolean_Data));
                  when ML_Types.Float_Type =>
                     Put (Float'Image (Data_Record.Real_Data));
                  when ML_Types.Integer_Type =>
                     Put (Integer'Image (Data_Record.Integer_Data));
                  when ML_Types.UB_String_Type =>
                     Put (Data_Record.UB_String_Data);
                  end case;
                  Put (", ");
               end;
               Next (Data_Curs);
            end loop;
            New_Line;
         end if;
         New_Line;
         Next (List_Curs);
      end loop;

   end Print_Data;

   --  -------------------------------------------------------------------------

   procedure Print_Description (Data : ARFF_Record) is
      use ML_Types;
      use Indefinite_String_Package;
      Header : constant ARFF_Header_Record := Data.Header;
      Info   : constant ARFF_Header := Header.Info;
      Curs   : Indefinite_String_Package.Cursor := Info.First;
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
