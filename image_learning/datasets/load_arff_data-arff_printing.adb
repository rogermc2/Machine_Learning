
with Ada.Containers;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;

package body Load_ARFF_Data.ARFF_Printing is

   --  -------------------------------------------------------------------------

   procedure Print_ARFF (Data : ARFF_Record) is
   begin
      Print_Description (Data);
      Put_Line ("Relation: " & Data.Header.Relation);
      Print_Attributes (Data);
      Print_Data (Data);
      Print_Target (Data);
      New_Line;

   end Print_ARFF;

   --  -------------------------------------------------------------------------

   procedure Print_Attributes (Data : ARFF_Record) is
      use Ada.Containers;
      use Ada.Strings;
      use Attribute_Data_Package;
      use Nominal_Data_Package;
      Header     : constant ARFF_Header_Record := Data.Header;
      Attributes : constant Attribute_List := Header.Attributes;
      Attribute  : Attribute_Record;
      Curs       : Attribute_Data_Package.Cursor := Attributes.First;
      Nom_Curs   : Nominal_Data_Package.Cursor;
      Count      : Count_Type;
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
            Count := 0;
            Put_Line ("Nominal attributes:");
            Nom_Curs := Attribute.Nominal_Data.First;
            while Has_Element (Nom_Curs) loop
               Count := Count + 1;
               declare
                  Nom_Value : constant Nominal_Data_Record :=
                                Element (Nom_Curs);
               begin
                  case Nom_Value.Data_Kind is
                  when Nominal_Integer =>
                     Put (Integer'Image (Nom_Value.Integer_Data));
                  when Nominal_Numeric | Nominal_Real =>
                     Put (Float'Image (Nom_Value.Real_Data));
                  when Nominal_String =>
                     Put (Nom_Value.UB_String_Data);
                  end case;
               end;

               if Count < Attribute.Nominal_Data.Length then
                  Put (", ");
               end if;
               Next (Nom_Curs);
            end loop;
            New_Line;
         end if;
         Next (Curs);
      end loop;

   end Print_Attributes;

   --  -------------------------------------------------------------------------

   procedure Print_Attributes (Text : String; Data : Attribute_List) is
      use Ada.Containers;
      use Ada.Strings;
      use Attribute_Data_Package;
      use Nominal_Data_Package;
      Attribute  : Attribute_Record;
      Curs       : Attribute_Data_Package.Cursor := Data.First;
      Nom_Curs   : Nominal_Data_Package.Cursor;
      Count      : Count_Type;
   begin
      New_Line;
      Put_Line (Text & ":");
      while Has_Element (Curs) loop
         Attribute := Element (Curs);
         Put (Trim (Attribute.Name, Both) & " ");
         Ada.Text_IO.Unbounded_IO.Put_Line
           (Trim (To_Unbounded_String
            (ARFF_Data_Type'Image (Attribute.Data_Kind)), Both));
         if not Attribute.Nominal_Data.Is_Empty then
            Count := 0;
            Put_Line ("Nominal attributes:");
            Nom_Curs := Attribute.Nominal_Data.First;
            while Has_Element (Nom_Curs) loop
               Count := Count + 1;
               declare
                  Nom_Value : constant Nominal_Data_Record :=
                                Element (Nom_Curs);
               begin
                  case Nom_Value.Data_Kind is
                  when Nominal_Integer =>
                     Put (Integer'Image (Nom_Value.Integer_Data));
                  when Nominal_Numeric | Nominal_Real =>
                     Put (Float'Image (Nom_Value.Real_Data));
                  when Nominal_String =>
                     Put (Nom_Value.UB_String_Data);
                  end case;
               end;

               if Count < Attribute.Nominal_Data.Length then
                  Put (", ");
               end if;
               Next (Nom_Curs);
            end loop;
            New_Line;
         end if;
         Next (Curs);
      end loop;
      New_Line;

   end Print_Attributes;

   --  -------------------------------------------------------------------------

   procedure Print_Data (Data : ARFF_Record; Start : Positive := 1;
                         Last : Positive := 10) is
      use AR_Real_Package_2D;
      use AR_Real_Package;
      Data_List_2D : constant AR_Real_List_2D := Data.Data;
      List_Curs    : AR_Real_Package_2D.Cursor := Data_List_2D.First;
      Data_List    : AR_Real_List;
      Data_Curs    : AR_Real_Package.Cursor;
      Count        : Natural := Start - 1;
      Count2       : Natural := 0;
   begin
      New_Line;
      Put_Line ("Dataset data:");
      Put_Line ("Data length:" & Integer'Image (Integer (Data_List_2D.Length)));
      while Has_Element (List_Curs) and Count <= Last loop
         Count := Count + 1;
         Data_List := Element (List_Curs);

         if Count >= Start and then Count <= Last then
            Put_Line ("Data row:" & Integer'Image (Count));
            Count2 := 0;
            Data_Curs := Data_List.First;
            while Has_Element (Data_Curs) loop
               Count2 := Count2 + 1;
               Put (Float'Image (Element (Data_Curs)));
               if Count2 <= Last then
                  Put (", ");
               end if;
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
      use IL_Types;
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

   procedure Print_Target (Data : ARFF_Record; Start : Positive := 1;
                           Last : Positive := 10) is
      use AR_Integer_Package;
      Target_List : constant AR_Integer_List := Data.Target;
      Target_Curs : AR_Integer_Package.Cursor := Target_List.First;
      Count       : Natural := Start - 1;
   begin
      New_Line;
      Put_Line ("Dataset target:");
      Put_Line ("Target length:" & Integer'Image (Integer (Target_List.Length)));

      while Has_Element (Target_Curs) loop
         Count := Count + 1;
         Put (Integer'Image (Element (Target_Curs)));
         if Count <= Last then
            Put (" ");
         end if;
         Next (Target_Curs);
      end loop;
      New_Line;

   end Print_Target;

   --  -------------------------------------------------------------------------

end Load_ARFF_Data.ARFF_Printing;