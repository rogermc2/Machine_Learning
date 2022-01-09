
with Ada.Strings;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;

with GNATCOLL.JSON; use GNATCOLL.JSON;

with AWS.Client;
with AWS.Response;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;

procedure Test_JSON_File is
   File           : File_Type;
   Response       : AWS.Response.Data;
   JSON_Data      : Unbounded_String;
   Reply_Type     : Unbounded_String;
   JSON_Main_Node : JSON_Value := Create;

   procedure Report_Inner_Field_Name (Name : Utf8_String; Value : JSON_Value);

   --  -------------------------------------------------------------------------

   procedure Report_Outer_Field_Name (Name : Utf8_String; Value : JSON_Value) is
      J_Val : String := Get (Value);
   begin
      Put_Line ("Outer_Field: " & Name & ": " & J_Val);
      Map_JSON_Object (Value, Report_Inner_Field_Name'Access);
   end Report_Outer_Field_Name;

   --  -------------------------------------------------------------------------

   procedure Report_Inner_Field_Name (Name : Utf8_String; Value : JSON_Value)
   is
   begin
      Put_Line ("Report_Inner_Field_Name field: " & Name);
      Put_Line (" field: " & Name & ", kind: " & Kind (Value)'Image);
   end Report_Inner_Field_Name;

   --  -------------------------------------------------------------------------

begin
   Put_Line ("JSON Dataset Name: ");
   New_Line;
   declare
      File_Name : String := Get_Line & ".json";
   begin
      Open (File, In_File, File_Name);
      JSON_Data := To_Unbounded_String (Get_Line (File));
      Close (File);
   end;

   JSON_Main_Node := Read (JSON_Data, Filename => "");
   Put_Line ("Map JSON_Main_Node");
   Map_JSON_Object (JSON_Main_Node, Report_Outer_Field_Name'Access);

end Test_JSON_File;
