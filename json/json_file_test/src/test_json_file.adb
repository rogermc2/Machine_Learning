
with Ada.Strings;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;

with GNATCOLL.JSON; use GNATCOLL.JSON;

with AWS.Client;
with AWS.Response;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;
with Ada.Command_Line;

with Html_Processor;
with Configure_AWS;
with Html_Processor;


procedure Test_JSON_File is
    use  Ada.Command_Line;
    use Configure_AWS;
--      URL1          : String := "http://www.openml.org/api/v1/json/data/554";
    Res           : AWS.Response.Data;
    Page_Raw_Data : Unbounded_String;
    CL_Config     : Config_Maps.Map;
begin
    Put_Line ("URL: ");
    declare
        URL : String := Get_Line;
    begin
        Res := AWS.Client.Get (URL);
    end;
    New_Line;

    Page_Raw_Data := AWS.Response.Message_Body (Res);
    Html_Processor.Process_Request (CL_Config, Page_Raw_Data);

end Test_JSON_File;
