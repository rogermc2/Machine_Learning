
with Ada.Command_Line; use Ada.Command_Line;

with AWS.Client;
with AWS.Response;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with Configure_AWS;
with Html_Processor;

procedure Test_HTML is
    Res           : AWS.Response.Data;
    Page_Raw_Data : Unbounded_String;
    CL_Config     : Configure_AWS.Config_Maps.Map;
    URL1          : constant String := "http://www.openml.org/api/v1/json/" &
                      "data/list/data_name/mnist_784" & "/limit/2/data_version/1";
    URL3          : constant String :=
      "http://www.openml.org/api/v1/json/data/554";
begin
    Res := AWS.Client.Get (URL1
    );
    Page_Raw_Data := AWS.Response.Message_Body (Res);
    Html_Processor.Process_Request (CL_Config, Page_Raw_Data);

end Test_HTML;
