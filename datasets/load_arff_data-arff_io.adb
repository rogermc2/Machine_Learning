
with Ada.Streams.Stream_IO;
with Ada.Text_IO; use Ada.Text_IO;

package body Load_ARFF_Data.ARFF_IO is


   LF : String (1 .. 1);

   procedure Write_Attributes (File_Name  : String;
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
      Write_Attributes (File_Name, Data.Header.Attributes);
      Close (File_ID);
      pragma Unreferenced (File_ID);

      Put_Line (Routine_Name & File_Name & " written");

   end Save_ARFF;

   --  -------------------------------------------------------------------------

   procedure Write_Attributes (File_Name  : String;
                               Attributes : Attribute_List) is

   begin
      null;
   end Write_Attributes;

   --  -------------------------------------------------------------------------

begin
   LF (1) := ASCII.LF;

end Load_ARFF_Data.ARFF_IO;
