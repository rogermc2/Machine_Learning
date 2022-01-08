
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;

with GNATCOLL.JSON; use GNATCOLL.JSON;

with Aws;
with Aws.Client;
with Aws.Response;

procedure Test_JSON is
   URL1           : constant String := "http://api.exmo.com/v1/pair_settings";
   URL2           : constant String
     := "http://www.openml.org/api/v1/json/data/list/data_name/mnist_784/limit/2/data_version/1";
   Aws_Reply      : Aws.Response.Data;
   JSON_Main_Node : JSON_Value := Create;

   procedure Report_Inner_Field_Name (Name : Utf8_String; Value : JSON_Value);

   --  -------------------------------------------------------------------------

   procedure Report_Outer_Field_Name (Name : Utf8_String; Value : JSON_Value) is
   begin
      Put_Line ("field: " & Name);
      Map_JSON_Object (Value, Report_Inner_Field_Name'Access);
   end Report_Outer_Field_Name;

   --  -------------------------------------------------------------------------

   procedure Report_Inner_Field_Name (Name : Utf8_String; Value : JSON_Value)
   is
   begin
      Put_Line (" field: " & Name & ", kind: " & Kind (Value)'Image);
   end Report_Inner_Field_Name;

   --  -------------------------------------------------------------------------

begin
   Aws_Reply := Aws.Client.Get (URL1);
   Put_Line ("Data type: " & Aws.Response.Content_Type (Aws_Reply));
   New_Line;
   JSON_Main_Node := Read
     (Strm => Unbounded_String'(Aws.Response.Message_Body(Aws_Reply)),
      Filename => "");

   Map_JSON_Object (JSON_Main_Node, Report_Outer_Field_Name'Access);

end Test_JSON;
