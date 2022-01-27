
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded; use  Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Text_IO.Unbounded_IO;

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
      J_Kind      : constant String :=
                      Fixed.Trim (JSON_Value_Type'Image (Value.Kind), Both);
      J_Array     : JSON_Array := Empty_Array;
      Inner_Array : JSON_Array := Empty_Array;
      Index       : Positive := 1;
      Inner_Index : Positive := 1;
      aValue      : JSON_Value;
      Inner_Value : JSON_Value;
      aLine       : Unbounded_String;
   begin
      Put_Line ("Outer_Field Value kind: " & J_Kind);

      if J_Kind = "JSON_STRING_TYPE" then
         Put_Line (Name & ": " & Get (Value) & "  Value kind: " & J_Kind);

      elsif J_Kind = "JSON_OBJECT_TYPE" then
         Map_JSON_Object (Value, Print_Inner_Field'Access);

      elsif J_Kind = "JSON_ARRAY_TYPE" then
         J_Array := Get (Value);
         Put_Line ("Array length " & Natural'Image (Length (J_Array)));
         Index := Array_First (J_Array);
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
               declare
                  Inner_Kind  : constant String
                    := Fixed.Trim (JSON_Value_Type'Image
                                   (Get (J_Array, 1).Kind), Both);
               begin
                  Put_Line ("Inner_Field Value kind: " & Inner_Kind);
                  Inner_Array := Get (aValue);
                  Inner_Index := 1;
                  while Array_Has_Element (Inner_Array, Inner_Index) loop
                     Put_Line ("Inner_Index: " & Integer'Image (Inner_Index));
                     Inner_Value := Array_Element (Inner_Array, Inner_Index);
                     Map_JSON_Object (Inner_Value, Print_Inner_Field'Access);
                     Inner_Index := Array_Next (Inner_Array, Inner_Index);
                  end loop;
               end;
            end if;

            Index := Array_Next (J_Array, Index);
         end loop;

      else
         Put_Line ("Field kind: " & Kind (Value)'Image);
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

   Result := ARFF.Load (To_String (Data), ARFF.Arff_Dense);
   New_Line;
   Put_Line (Routine_Name & "Result Outer_Field:");
   Map_JSON_Object (Result, Print_Outer_Field'Access);

end Test_ARFF;
