
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded; use  Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Text_IO.Unbounded_IO;

with GNATCOLL.JSON; use GNATCOLL.JSON;

with ARFF_Json;

procedure Test_ARFF is
   Routine_Name : constant String := "Test_ARFF ";
   File_Name    : constant String := "iris.arff";
   File         : File_Type;
   Data         : Unbounded_String := To_Unbounded_String ("");
   Result       : JSON_Value;

   procedure Print_Inner_Field (Name : Utf8_String; Value : JSON_Value);

   --  -------------------------------------------------------------------------

   procedure Print_Array (Name : Utf8_String; Value : JSON_Value) is
      J_Array : constant JSON_Array := Get (Value);
      aValue  : JSON_Value;
      aLine   : Unbounded_String;
      Index   : Positive := Array_First (J_Array);
   begin
      while Array_Has_Element (J_Array, Index) loop
         aValue := Array_Element (J_Array, Index);
         if Has_Field (aValue, "text") then
            aLine := Get (aValue, "text");
            Unbounded_IO.Put_Line (aLine);

         elsif Has_Field (aValue, "attributes") then
            Put_Line ("attributes");

         elsif Has_Field (aValue, "data") then
            Put_Line ("data");

         elsif Kind (aValue) = JSON_Array_Type then
            Put_Line ("Inner JSON_Array_Type");
            Print_Array (Name, aValue);

         elsif Kind (aValue) = JSON_Object_Type then
            if Kind (Get (aValue, "values")) = JSON_Array_Type then
               New_Line;
               Print_Array (Name, Get (aValue, "values"));
            else
               Map_JSON_Object (aValue, Print_Inner_Field'Access);
            end if;

         else
            Put_Line ("Invalid field kind: " & Kind (Value)'Image);
         end if;

         Index := Array_Next (J_Array, Index);
      end loop;

   end Print_Array;

   --  -------------------------------------------------------------------------

   procedure Print_Inner_Field (Name : Utf8_String; Value : JSON_Value) is
   begin
      Put ("    Field: " & Name & "   ");
      Put_Line (Value.Write);

   end Print_Inner_Field;

   --  -------------------------------------------------------------------------

   procedure Print_Outer_Field (Name : Utf8_String; Value : JSON_Value) is
      use Ada.Strings;
      J_Kind : constant String :=
                 Fixed.Trim (JSON_Value_Type'Image (Value.Kind), Both);
      aValue : JSON_Value;
   begin
--        Put_Line ("Outer_Field Value kind: " & J_Kind);

      if J_Kind = "JSON_STRING_TYPE" then
         Put_Line (Name & ": " & Get (Value) & "  Value kind: " & J_Kind);

      elsif J_Kind = "JSON_OBJECT_TYPE" then
         Map_JSON_Object (Value, Print_Inner_Field'Access);

      elsif J_Kind = "JSON_ARRAY_TYPE" then
         Print_Array (Name, Value);
      else
         Put_Line ("Unknown inner JSON field kind: " &
                     JSON_Value_Type'Image (Kind (aValue)));
         Put_Line (aValue.Write);
      end if;

      New_Line;

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

   Result := ARFF_Json.Load (To_String (Data), ARFF_Json.Arff_Dense);
   New_Line;
   Put_Line (Routine_Name & "Result Outer_Field:");
   Map_JSON_Object (Result, Print_Outer_Field'Access);

end Test_ARFF;