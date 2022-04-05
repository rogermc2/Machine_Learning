--  From https://franckbehaghel.eu/programming/ada/ada-web-server-example/
--       http_req.php

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with Configure_AWS;

package Html_Processor is
   use Ada.Strings;
   use Configure_AWS;

   type Parser_State is (Init, Html_Tag_Start, Html_Tag_End, Html_Tag,
                         Script_Tag_Start, Script_Tag_End, Script_Tag);

   procedure Process_Request (Request : String);
   procedure Process_Request (Request : Unbounded_String);

   procedure Process_Request (Config : Config_Maps.Map; Request : String);
   procedure Process_Request (Config  : Config_Maps.Map;
                              Request : Unbounded_String);

end Html_Processor;
