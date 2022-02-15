
with Ada.Assertions; use Ada.Assertions;
with Ada.Streams.Stream_IO;
with Ada.Text_IO; use Ada.Text_IO;

package body Load_ARFF_Data.ARFF_IO is


   LF : String (1 .. 1);

   procedure Write_Attributes
     (Data_Stream : Ada.Streams.Stream_IO.Stream_Access;
      Attributes : Attribute_List);

   --  -------------------------------------------------------------------------

   procedure Save_ARFF (File_Name : String; Data : ARFF_Record) is
      use Ada.Streams.Stream_IO;
      Routine_Name : constant String := "Load_ARFF_Data.Save_ARFF ";
      File_ID      : Ada.Streams.Stream_IO.File_Type;
      Data_Stream  : Stream_Access;
   begin
      Create (File_ID, Out_File, File_Name);
      Data_Stream := Stream (File_ID);
      ARFF_Header'Write (Data_Stream, Data.Header.Info);
      String'Write (Data_Stream, LF & "Relation: " &
                      To_String (Data.Header.Relation));
      Write_Attributes (Data_Stream, Data.Header.Attributes);
      Close (File_ID);
      pragma Unreferenced (File_ID);

      Put_Line (Routine_Name & File_Name & " written");

   end Save_ARFF;

   --  -------------------------------------------------------------------------

   procedure Write_Attributes
     (Data_Stream : Ada.Streams.Stream_IO.Stream_Access;
      Attributes : Attribute_List) is
      use Attribute_Data_Package;
      Routine_Name : constant String := "Load_ARFF_Data.Write_Attributes ";
      Curs : Attribute_Data_Package.Cursor := Attributes.First;
      Item : Attribute_Record;
   begin
      Assert (not Is_Empty (Attributes), Routine_Name &
                "Attributes list is empty");
      Put_Line (Routine_Name & "saving Attributes");

      while Has_Element (Curs) loop
         Item := Element (Curs);
         String'Write (Data_Stream, To_String (Item.Name));
         Put_Line (Routine_Name & "Name: " & To_String (Item.Name));
         ARFF_Data_Type'Write (Data_Stream, Item.Data_Kind);
         ML_Types.Indef_String_List'Write (Data_Stream, Item.Nominal_Names);
         Nominal_Types_List'Write (Data_Stream, Item.Nominal_Types);
         Next (Curs);
      end loop;

   end Write_Attributes;

   --  -------------------------------------------------------------------------

begin
   LF (1) := ASCII.LF;

end Load_ARFF_Data.ARFF_IO;
