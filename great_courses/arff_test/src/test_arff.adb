
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded; use  Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;

with GNATCOLL.JSON; use GNATCOLL.JSON;

with ARFF;

procedure Test_ARFF is
   Routine_Name : constant String := "Test_ARFF ";
   File_Name    : constant String := "iris.arff";
   File         : File_Type;
   Data         : Unbounded_String := To_Unbounded_String ("");
   Result       : JSON_Value;

   procedure Print_Inner_Field (Name : Utf8_String; Value : JSON_Value)
   is
   begin
      Put_Line ("Inner_Field Name: " & Name);
      Put_Line (" field: " & Name & ", kind: " & Kind (Value)'Image);
   end Print_Inner_Field;

   --  -------------------------------------------------------------------------

   procedure Print_Outer_Field (Name : Utf8_String; Value : JSON_Value) is
      use Ada.Strings;
      J_Kind : constant String :=
                 Fixed.Trim (JSON_Value_Type'Image (Value.Kind), Both);
   begin
      if J_Kind = "JSON_STRING_TYPE" then
         Put_Line ("Outer_Field: " & Name & ": " & Get (Value) &
                     "  Value kind: " & J_Kind);

      elsif J_Kind = "JSON_OBJECT_TYPE" then
         Map_JSON_Object (Value, Print_Inner_Field'Access);
      else
         Put_Line ("Outer_Field Value kind: " & J_Kind);
      end if;

   end Print_Outer_Field;

   --  -------------------------------------------------------------------------

begin
   Put_Line (Routine_Name);
   Open (File, In_File, File_Name);
   while not End_Of_File (File) loop
      Data := Data & To_Unbounded_String (Get_Line (File));
      Data := Data & "\r\n";
   end loop;
   Close (File);

   Result := ARFF.Load (To_String (Data), ARFF.Arff_Dense);

   Map_JSON_Object (Result, Print_Outer_Field'Access);

end Test_ARFF;
