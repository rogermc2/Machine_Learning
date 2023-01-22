
with Ada.Containers;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO;

with Util.Serialize.IO.JSON;
with Util.Serialize.Mappers.Vector_Mapper;
with Util.Streams.Texts;
with Util.Streams.Buffered; use Util.Streams.Buffered;

with Mapping; use Mapping;

procedure Test_JSON_Reader is
   use type Ada.Containers.Count_Type;

   package Person_Vector_Mapper is
     new Util.Serialize.Mappers.Vector_Mapper (Vectors        => Person_Vector,
                                               Element_Mapper => Person_Mapper);
   Reader : Util.Serialize.IO.JSON.Parser;
   Mapper : Util.Serialize.Mappers.Processing;
   List   : aliased Mapping.Person_Vector.Vector;
   Person : aliased Mapping.Person_Data;

   File_Name             : constant String := "person.json";
   --  Mapping for a list of Person records (stored as a Vector).
   Person_Vector_Mapping : aliased Person_Vector_Mapper.Mapper;

   procedure Print (aPerson : in Mapping.Person_Vector.Cursor);

   --  -------------------------------------------------------------------------

   procedure Print (aPerson : in Mapping.Person_Data) is
   begin
      Ada.Text_IO.Put_Line ("Name       : " & To_String (aPerson.Name));
      Ada.Text_IO.Put_Line ("first_name : " & To_String (aPerson.First_Name));
      Ada.Text_IO.Put_Line ("last_name  : " & To_String (aPerson.Last_Name));
      Ada.Text_IO.Put_Line ("Age        : " & Natural'Image (aPerson.Age));
      Ada.Text_IO.Put_Line ("Street     : " & To_String (aPerson.Addr.Street));
      Ada.Text_IO.Put_Line ("City       : " & To_String (aPerson.Addr.City));
      Ada.Text_IO.Put_Line ("Zip        : " & Natural'Image (aPerson.Addr.Zip));
      Ada.Text_IO.Put_Line ("Country    : " & To_String (aPerson.Addr.Country));
      Ada.Text_IO.Put_Line ("Info       : " & To_String (aPerson.Addr.Info.Name)
                            & "=" & To_String (aPerson.Addr.Info.Value));
   end Print;

   --  -------------------------------------------------------------------------

   procedure Print (aPerson : in Mapping.Person_Vector.Cursor) is
   begin
      Print (Mapping.Person_Vector.Element (aPerson));
   end Print;

   --  -------------------------------------------------------------------------

begin
   Person_Vector_Mapping.Set_Mapping (Mapping.Get_Person_Mapper);
   Mapper.Add_Mapping ("/list", Person_Vector_Mapping'Unchecked_Access);
   Mapper.Add_Mapping ("/person", Mapping.Get_Person_Mapper.all'Access);

   Person_Vector_Mapper.Set_Context (Mapper, List'Unchecked_Access);
   Mapping.Person_Mapper.Set_Context (Mapper, Person'Unchecked_Access);
   Reader.Parse (File_Name, Mapper);

   --  The list now contains our elements.
   List.Iterate (Process => Print'Access);
   if List.Length = 0 then
      Print (Person);
   end if;

   declare
      Buffer : aliased Util.Streams.Buffered.Output_Buffer_Stream;
      Print  : aliased Util.Streams.Texts.Print_Stream;
      Output : Util.Serialize.IO.JSON.Output_Stream;
   begin
      Buffer.Initialize (Size => 10000);
      Print.Initialize (Buffer'Unchecked_Access);
      Output.Initialize (Print'Unchecked_Access);

      Mapping.Get_Person_Mapper.Write (Output, Person);
      Ada.Text_IO.Put_Line ("Person: "
                            & Util.Streams.Texts.To_String (Print));
   end;

   --     declare
   --        Buffer : aliased Util.Streams.Buffered.Output_Buffer_Stream;
   --        Print  : aliased Util.Streams.Texts.Print_Stream;
   --        Output : Util.Serialize.IO.JSON.Output_Stream;
   --     begin
   --        Buffer.Initialize (Size => 10000);
   --        Print.Initialize (Buffer'Unchecked_Access);
   --        Output.Initialize (Print'Unchecked_Access);
   --        Output.Write ("{""list"":");
   --        Person_Vector_Mapping.Write (Output, List);
   --        Output.Write ("}");
   --
   --        Ada.Text_IO.Put_Line ("IO:");
   --        Ada.Text_IO.Put_Line (Util.Streams.Texts.To_String (Print));
   --     end;

   --  -------------------------------------------------------------------------

end Test_JSON_Reader;
