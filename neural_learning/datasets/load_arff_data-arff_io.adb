
with Ada.Streams.Stream_IO;
with Ada.Text_IO; use Ada.Text_IO;

package body Load_ARFF_Data.ARFF_IO is

   --  -------------------------------------------------------------------------

   procedure Read_ARFF_Ada (File_Name : String; Data : out ARFF_Record) is
      use Ada.Streams.Stream_IO;
      Routine_Name : constant String := "Load_ARFF_Data.Read_ARFF_Ada ";
      File_ID      : Ada.Streams.Stream_IO.File_Type;
      Data_Stream  : Stream_Access;
   begin
      Open (File_ID, In_File, File_Name);
      Data_Stream := Stream (File_ID);
      ARFF_Record'Read (Data_Stream, Data);
      Close (File_ID);
      pragma Unreferenced (File_ID);

      Put_Line (Routine_Name & File_Name & " loaded");

   end Read_ARFF_Ada;

   --  -------------------------------------------------------------------------

   procedure Save_ARFF (File_Name : String; Data : ARFF_Record) is
      use Ada.Streams.Stream_IO;
      Routine_Name : constant String := "Load_ARFF_Data.Save_ARFF ";
      File_ID      : Ada.Streams.Stream_IO.File_Type;
      Data_Stream  : Stream_Access;
   begin
      Create (File_ID, Out_File, File_Name);
      Data_Stream := Stream (File_ID);
      ARFF_Record'Write (Data_Stream, Data);
      Close (File_ID);
      pragma Unreferenced (File_ID);

      Put_Line (Routine_Name & File_Name & " written");

   end Save_ARFF;

   --  -------------------------------------------------------------------------

end Load_ARFF_Data.ARFF_IO;
